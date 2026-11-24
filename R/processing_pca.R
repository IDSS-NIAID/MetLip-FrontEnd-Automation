# Exploratory PCA for completed processing runs. No additional packages beyond
# the existing app dependencies (stats, ggplot2, shiny, DT and zip).
# Rows = selected fitting samples; columns = lipid features.

pca_qc_choices <- c("Study samples only (exclude QCs)" = "exclude",
  "Study fit + QC projections (QCs do not affect fit)" = "project",
  "Fit study samples + eligible QCs together" = "include")

pca_sample_scope <- function(qc_mode) switch(qc_mode,
  exclude = "Fit on study samples only; QCs excluded from PCA",
  project = "Fit on study samples; eligible QCs projected with fixed study preprocessing",
  include = "Fit on study samples and eligible QCs together; means, SDs and loadings use both")

pca_scope_caption <- function(pca) {
  paste0(pca$settings$sample_scope, ". Color and shape change only the display.")
}

# Projection uses only retained PCA features. Exclusion never reads QC values.
pca_qc_matrix <- function(qc, features, study_ids) {
  if (!is.matrix(qc) || !is.numeric(qc)) stop("PCA QC input must be a numeric matrix.")
  ids <- rownames(qc)
  if (is.null(ids) || anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    stop("PCA QC sample IDs must be present and unique.")
  if (any(ids %in% study_ids)) stop("PCA study and QC sample IDs must be distinct.")
  if (is.null(colnames(qc)) || anyDuplicated(colnames(qc)) || !all(features %in% colnames(qc)))
    stop("PCA QC input must contain each required feature exactly once.")
  qc <- qc[, features, drop = FALSE]
  if (any(!is.finite(qc))) stop("PCA QC measurements must be finite; no extra imputation was performed.")
  qc
}

pca_stage_matrix <- function(result, stage = c("tsn", "imputed")) {
  stage <- match.arg(stage)
  dat <- if (stage == "tsn") result$normalized_long else result$imputed
  value <- if (stage == "tsn") { if ("Value_normalized" %in% names(dat)) "Value_normalized" else "Value_tsn" } else "Value_imputed"
  needed <- c("Acquired_Sample_ID", "Lipid", value)
  if (!is.data.frame(dat) || !all(needed %in% names(dat)) || !nrow(dat))
    stop("PCA requires a completed, nonempty processed data table.")
  ids <- as.character(dat$Acquired_Sample_ID)
  lipids <- as.character(dat$Lipid)
  if (anyNA(ids) || anyNA(lipids) || any(!nzchar(trimws(ids))) || any(!nzchar(trimws(lipids))))
    stop(analysis_text("PCA sample and lipid IDs must be nonmissing and nonempty.", result))
  if (anyDuplicated(dat[c("Acquired_Sample_ID", "Lipid")]))
    stop(analysis_text("Duplicate sample/lipid pairs in the PCA input; no averaging was performed.", result))
  if (!is.numeric(dat[[value]])) stop("PCA measurements must be numeric.")
  samples <- unique(ids)
  features <- unique(lipids)
  x <- matrix(NA_real_, length(samples), length(features), dimnames = list(samples, features))
  x[cbind(match(ids, samples), match(lipids, features))] <- dat[[value]]
  if (any(!is.finite(x)))
    stop("PCA requires a complete finite matrix. Missing pairs, NA/NaN or infinite values remain; no extra imputation was performed.")
  x
}

fit_lipid_pca <- function(result, stage = c("tsn", "imputed"),
                          transform = c("log2", "none"),
                          scaling = c("pareto", "unit", "none", "as_is"),
                          qc_mode = c("project", "exclude", "include")) {
  stage <- match.arg(stage)
  transform <- match.arg(transform)
  scaling <- match.arg(scaling)
  qc_mode <- match.arg(qc_mode)
  if (!is.null(result$analysis_error)) stop(result$analysis_error)
  centered <- scaling != "as_is"
  x <- pca_stage_matrix(result, stage)
  if (nrow(x) < 2L) stop("PCA requires at least two retained study samples.")
  study_ids <- rownames(x)
  qc <- if (stage == "tsn") result$qc_normalized else result$qc_imputed
  n_qc <- if (is.null(qc)) 0L else NROW(qc)
  qc_ids <- character()
  if (qc_mode == "include" && n_qc > 0L) {
    qc <- pca_qc_matrix(qc, colnames(x), study_ids)
    qc_ids <- rownames(qc)
    x <- rbind(x, qc)
  }
  n_input <- ncol(x)
  if (transform == "log2") {
    if (any(x <= 0)) stop("Log2 PCA requires strictly positive values; no pseudocount was added.")
    x <- log2(x)
  }
  if (!centered && !any(x != 0)) stop("PCA requires at least one nonzero measurement when centering is off.")
  feature_sd <- apply(x, 2L, stats::sd)
  # Relative numerical tolerance, evaluated on the chosen transformed scale.
  cutoff <- 100 * .Machine$double.eps * apply(abs(x), 2L, max)
  keep <- if (centered) is.finite(feature_sd) & feature_sd > cutoff else rep(TRUE, ncol(x))
  removed <- data.frame(Lipid = colnames(x)[!keep],
    Reason = rep("Constant or numerically constant after transformation", sum(!keep)),
    SD = unname(feature_sd[!keep]), Tolerance = unname(cutoff[!keep]), check.names = FALSE)
  if (!any(keep)) stop(analysis_text("No variable lipid features remain for PCA after transformation.", result))
  x <- x[, keep, drop = FALSE]
  feature_sd <- feature_sd[keep]
  divisor <- switch(scaling, as_is = rep(1, ncol(x)), none = rep(1, ncol(x)), unit = feature_sd,
    pareto = sqrt(feature_sd))
  names(divisor) <- colnames(x)
  model <- stats::prcomp(x, center = centered, scale. = if (centered) divisor else FALSE,
    tol = sqrt(.Machine$double.eps))
  if (!ncol(model$rotation)) stop("No numerically nonzero principal components remain.")
  # Anchor each sign to its largest absolute loading for reproducible labels.
  # Sign orientation has no effect on distances, variance or reconstruction.
  for (j in seq_len(ncol(model$rotation))) {
    anchor <- which.max(abs(model$rotation[, j]))
    if (model$rotation[anchor, j] < 0) {
      model$rotation[, j] <- -model$rotation[, j]
      model$x[, j] <- -model$x[, j]
    }
  }
  eigenvalues <- model$sdev^2
  if (!all(is.finite(eigenvalues)) || sum(eigenvalues) <= 0)
    stop("PCA produced invalid total variance. Inspect the data magnitudes.")
  # prcomp retains the full sdev spectrum even when tol omits null axes.
  # Use total variance across ALL singular values as the denominator.
  variance <- data.frame(PC = paste0("PC", seq_along(eigenvalues)),
    Eigenvalue = eigenvalues, Variance_percent = 100 * eigenvalues / sum(eigenvalues),
    Cumulative_percent = 100 * cumsum(eigenvalues) / sum(eigenvalues),
    Available_for_plot = seq_along(eigenvalues) <= ncol(model$rotation))
  scores <- data.frame(Acquired_Sample_ID = rownames(model$x), model$x,
    check.names = FALSE, row.names = NULL)
  qc_analysis <- matrix(numeric(), 0, ncol(x), dimnames = list(NULL, colnames(x)))
  if (qc_mode == "project" && n_qc > 0L) {
    qc <- pca_qc_matrix(qc, colnames(x), study_ids)
    qc_ids <- rownames(qc)
    if (transform == "log2") {
      if (any(qc <= 0)) stop("Log2 QC projection requires strictly positive values; no pseudocount was added.")
      qc <- log2(qc)
    }
    if (any(!is.finite(qc))) stop("QC projection requires finite values under the selected transformation.")
    qc_analysis <- if (centered) scale(qc, center = model$center, scale = divisor) else qc
    qc_scores <- qc_analysis %*% model$rotation
    scores <- rbind(scores, data.frame(Acquired_Sample_ID = rownames(qc_scores), qc_scores,
      check.names = FALSE, row.names = NULL))
  }
  metadata <- result$metadata
  if (is.null(metadata)) metadata <- data.frame(Acquired_Sample_ID = scores$Acquired_Sample_ID)
  metadata <- as.data.frame(metadata, check.names = FALSE)
  if (anyDuplicated(as.character(metadata[[1]]))) stop("Duplicate metadata IDs.")
  index <- match(scores$Acquired_Sample_ID, as.character(metadata[[1]]))
  metadata <- metadata[index, , drop = FALSE]
  metadata[[1]] <- scores$Acquired_Sample_ID
  rownames(metadata) <- NULL
  imputed_fraction <- if (".Imputed_fraction" %in% names(metadata)) metadata$.Imputed_fraction else rep(0, nrow(metadata))
  qc_role <- if (qc_mode == "include") "QC fitted" else "QC projected"
  metadata$.Role <- ifelse(scores$Acquired_Sample_ID %in% study_ids, "Study",
    ifelse(!is.na(imputed_fraction) & imputed_fraction > 0, paste0(qc_role, " (imputed)"), qc_role))
  loadings <- data.frame(Lipid = rownames(model$rotation), model$rotation,
    check.names = FALSE, row.names = NULL)
  preprocessing <- data.frame(Lipid = colnames(x), Center = if (centered) unname(model$center) else rep(0, ncol(x)),
    SD_after_transform = unname(feature_sd), Scale_divisor = unname(divisor),
    check.names = FALSE)
  settings <- list(data_type = analysis_terms(result)$id, stage = if (identical(result$settings$processing_mode, "skip")) "prepared" else stage, transform = transform, center = centered, scaling = scaling,
    processing_mode = if (is.null(result$settings$processing_mode)) "process" else result$settings$processing_mode,
    variance_basis = if (centered) "Centered variance" else "Uncentered sum of squares",
    samples = nrow(x), study_samples = length(study_ids), eligible_qcs = n_qc,
    fitted_qcs = if (qc_mode == "include") length(qc_ids) else 0L,
    projected_qcs = nrow(qc_analysis), excluded_qcs = if (qc_mode == "exclude") n_qc else 0L,
    qc_mode = qc_mode, input_features = n_input, fitted_features = ncol(x),
    omitted_constant_features = sum(!keep), available_PCs = ncol(model$rotation),
    rank_tolerance = sqrt(.Machine$double.eps), constant_tolerance_multiplier = 100,
    additional_imputation = "none", sample_scope = if (qc_mode != "exclude" && n_qc == 0L)
      "Fit on study samples only; no eligible QCs available" else pca_sample_scope(qc_mode),
    normalization = if (is.null(result$selection)) stage else result$selection$method)
  list(model = model, scores = scores, loadings = loadings, variance = variance,
    metadata = metadata, preprocessing = preprocessing, removed_features = removed,
    settings = settings, analysis_matrix = if (centered) scale(x, center = model$center, scale = divisor) else x,
    qc_analysis_matrix = qc_analysis,
    processing_settings = active_processing_settings(result), provenance = result$provenance,
    completed_at = result$completed_at, app_version = result$app_version,
    qc_eligibility = result$qc_eligibility,
    session_info = utils::capture.output(utils::sessionInfo()))
}

pca_variance_label <- function(pca) {
  if (identical(pca$settings$center, FALSE)) "Uncentered sum of squares" else "Explained variance"
}

# Selection is a display policy; retain the complete fitted model and spectrum.
pca_selectable_components <- function(pca) {
  pcs <- colnames(pca$model$rotation)
  v <- pca$variance[match(pcs, pca$variance$PC), , drop = FALSE]
  pcs[which(v$Available_for_plot & is.finite(v$Variance_percent) & v$Variance_percent >= 5)]
}

pca_selected_axes <- function(pca, x = NULL, y = NULL) {
  pcs <- pca_selectable_components(pca)
  if (length(x) != 1L || !x %in% pcs) x <- if (length(pcs)) pcs[1L] else ""
  others <- setdiff(pcs, x)
  if (length(y) != 1L || !y %in% others) y <- if (length(others)) others[1L] else ""
  list(x = x, y = y)
}

pca_require_plot_components <- function(pca, kind, pairs = character()) {
  if (kind == "scree") return(invisible(NULL))
  pcs <- pca_selectable_components(pca)
  if (!length(pcs)) stop("No PCs meet the 5% cutoff. Component contributions, tables and model downloads remain available.")
  if (kind == "biplot" && length(pcs) < 2L)
    stop("Biplot requires two PCs that each meet the 5% cutoff.")
  if (kind == "pairs" && length(intersect(pairs, pcs)) < 2L)
    stop("Pairs requires two to six selected PCs that each meet the 5% cutoff.")
  invisible(NULL)
}

pca_variance_table <- function(pca) {
  table <- pca$variance
  if (identical(pca$settings$center, FALSE)) names(table)[match(
    c("Variance_percent", "Cumulative_percent"), names(table))] <- c("Sum_of_squares_percent", "Cumulative_percent")
  table
}

pca_axis_label <- function(pca, pc, quantity = "") {
  i <- match(pc, pca$variance$PC)
  if (nzchar(quantity))
    return(sprintf("%s %s (%.1f%% %s)", pc, quantity, pca$variance$Variance_percent[i], if (identical(pca$settings$center, FALSE)) "sum of squares" else "variance"))
  sprintf("%s (%.1f%%%s)", pc, pca$variance$Variance_percent[i], if (identical(pca$settings$center, FALSE)) " sum of squares" else "")
}

pca_plot_context <- function(pca) {
  paste(if (pca$settings$stage %in% c("tsn", "prepared")) paste("Normalization:", pca$settings$normalization) else "Filtered/imputed",
    if (pca$settings$transform == "log2") "log2" else "untransformed",
    if (identical(pca$settings$center, FALSE)) "No centering or scaling" else paste(pca$settings$scaling, "scaling; mean centered"), sep = " | ")
}

# Metadata headers are used verbatim; the first column is always the sample ID.
metadata_color_choices <- function(metadata, diagnostics = character()) {
  fields <- unique(c(names(metadata)[-1L], diagnostics))
  labels <- fields
  labels[fields == ".Role"] <- "Sample role"
  labels[fields == ".Imputed_fraction"] <- "Imputed fraction"
  labels <- c("None", labels)
  # Preserve uploaded labels when they coincide with an app-generated option.
  for (index in c(1L, which(fields %in% c(".Role", ".Imputed_fraction")) + 1L)) {
    while (labels[index] %in% labels[-index]) labels[index] <- paste0(labels[index], " (app)")
  }
  stats::setNames(c("", fields), labels)
}

metadata_color_selection <- function(metadata, column, fallback = "") {
  fields <- c("", names(metadata)[-1L])
  if (length(column) == 1L && !is.na(column) && column %in% fields) return(column)
  if (fallback %in% fields) fallback else ""
}

metadata_color_spec <- function(metadata, column, mode = "auto") {
  mode <- match.arg(mode, c("auto", "categorical", "continuous"))
  if (is.null(column) || identical(column, "")) return(list(values = NULL, mode = "categorical"))
  if (length(column) != 1L || is.na(column) || !column %in% names(metadata)[-1L])
    stop("Choose a valid metadata coloring field.")
  original <- metadata[[column]]
  v <- trimws(as.character(original))
  absent <- is.na(v) | v %in% c("", "NA", "N/A")
  z <- suppressWarnings(as.numeric(v))
  if (mode == "auto") mode <- if (any(!absent) && all(is.finite(z[!absent]))) "continuous" else "categorical"
  if (mode == "continuous") {
    if (any(!absent & !is.finite(z))) stop("Continuous coloring requires numeric values; choose categorical coloring for this field.")
    z[absent] <- NA_real_
    values <- z
  } else {
    # Keep true missing values separate from any literal metadata category.
    v[absent] <- NA_character_
    levels <- if (is.factor(original)) trimws(levels(original)) else v
    levels <- sort(unique(levels[!is.na(levels) & !levels %in% c("", "NA", "N/A")]))
    values <- factor(v, levels = levels)
  }
  list(values = values, mode = mode)
}

pca_color_data <- function(pca, column = "", mode = "auto") {
  d <- pca$scores
  at <- match(d$Acquired_Sample_ID, as.character(pca$metadata[[1L]]))
  d$.role <- if (".Role" %in% names(pca$metadata)) pca$metadata$.Role[at] else "Study"
  spec <- metadata_color_spec(pca$metadata, column, mode)
  if (!is.null(spec$values)) d$.color <- spec$values[at]
  d
}

pca_metadata_label <- function(column) {
  if (identical(column, ".Role")) "Sample role" else
    if (identical(column, ".Imputed_fraction")) "Imputed fraction" else column
}

# Explicit shapes avoid ggplot2's default six-category limit silently hiding points.
pca_shape_palette <- c(16, 17, 15, 18, 3, 7, 8, 0, 1, 2, 5, 6)

pca_shape_spec <- function(metadata, column = ".Role") {
  if (is.null(column) || identical(column, "")) return(NULL)
  if (length(column) != 1L || is.na(column) || !column %in% names(metadata)[-1L])
    stop("Choose a valid metadata shape field.")
  values <- droplevels(metadata_color_spec(metadata, column, "categorical")$values)
  if (nlevels(values) > length(pca_shape_palette))
    stop("Shape mapping supports at most 12 categories. Choose a field with fewer categories, or choose None; use color for continuous values.")
  list(values = values, shapes = stats::setNames(pca_shape_palette[seq_len(nlevels(values))], levels(values)))
}

pca_point_data <- function(pca, color = "", mode = "auto", shape = ".Role") {
  d <- pca_color_data(pca, color, mode)
  spec <- pca_shape_spec(pca$metadata, shape)
  if (!is.null(spec)) {
    at <- match(d$Acquired_Sample_ID, as.character(pca$metadata[[1L]]))
    d$.shape <- spec$values[at]
  }
  d
}

pca_point_layers <- function(d, color = "", shape = ".Role", size = 2.8, alpha = .85) {
  mapping <- if (nzchar(color) && nzchar(shape)) ggplot2::aes(color = .color, shape = .shape) else
    if (nzchar(color)) ggplot2::aes(color = .color) else
      if (nzchar(shape)) ggplot2::aes(shape = .shape) else ggplot2::aes()
  args <- list(mapping = mapping, size = size, alpha = alpha)
  if (!nzchar(color)) args$color <- "#176d80"
  if (!nzchar(shape)) args$shape <- 16
  layers <- list(do.call(ggplot2::geom_point, args))
  if (nzchar(color)) layers <- c(layers, list(pca_color_scale(color,
    if (is.numeric(d$.color)) "continuous" else "categorical")))
  if (nzchar(shape)) {
    shapes <- stats::setNames(pca_shape_palette[seq_len(nlevels(d$.shape))], levels(d$.shape))
    layers <- c(layers, list(ggplot2::scale_shape_manual(name = pca_metadata_label(shape),
      values = shapes, limits = c(levels(d$.shape), if (anyNA(d$.shape)) NA_character_),
      na.value = 4, na.translate = TRUE, drop = FALSE)))
  }
  layers
}

imputed_fraction_scale <- function(aesthetic = "colour") {
  # The default short colourbar crowds decimal fractions in comparison panels.
  # Labels are percentages only; the mapped data and exported fractions stay 0–1.
  ggplot2::scale_color_viridis_c(name = "Imputed fraction", aesthetics = aesthetic,
    na.value = "grey65", breaks = scales::breaks_extended(n = 3),
    labels = function(x) {
      # A constant range has no tick spacing from which to infer precision.
      if (length(unique(x[is.finite(x)])) == 1L)
        return(paste0(format(100 * x, digits = 3, trim = TRUE, scientific = FALSE), "%"))
      scales::label_percent()(x)
    },
    guide = ggplot2::guide_colorbar(direction = "horizontal", theme = ggplot2::theme(
      legend.key.width = grid::unit(4.5, "cm"), legend.key.height = grid::unit(.35, "cm"),
      legend.title.position = "top", legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9))))
}

pca_color_scale <- function(column, mode) {
  if (identical(column, ".Imputed_fraction") && mode == "continuous") return(imputed_fraction_scale())
  if (mode == "continuous") ggplot2::scale_color_viridis_c(name = pca_metadata_label(column), na.value = "grey65") else
    ggplot2::scale_color_discrete(name = pca_metadata_label(column), na.value = "grey65", drop = FALSE)
}

pca_theme <- function() {
  ggplot2::theme_bw(base_size = 12) + ggplot2::theme(
    legend.position = "bottom", legend.box = "vertical", panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot", plot.caption = ggplot2::element_text(hjust = 0),
    plot.margin = ggplot2::margin(12, 18, 12, 12))
}

# Match the clean scatter-plot styling in Script_1_PCA.R. Keep readable
# app-sized text and the existing bottom legends for long metadata fields.
pca_scatter_theme <- function() {
  pca_theme() + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = .5),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = .35),
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    plot.title.position = "panel",
    plot.title = ggplot2::element_text(hjust = .5),
    plot.subtitle = ggplot2::element_text(hjust = .5))
}

# Put both origin lines behind the data, without inheriting color/shape mappings.
pca_origin_layers <- function() list(
  ggplot2::geom_hline(yintercept = 0, colour = "grey35", linetype = "dashed", linewidth = .35),
  ggplot2::geom_vline(xintercept = 0, colour = "grey35", linetype = "dashed", linewidth = .35))

# Expand each axis around zero; never truncate points or rescale coefficients.
# Score and loading ranges remain independent, in their original PCA units.
pca_symmetric_limits <- function(limits) {
  extent <- max(abs(limits[is.finite(limits)]), 0)
  if (extent == 0) extent <- 1
  c(-extent, extent)
}

pca_scatter_scales <- function(two_axes = TRUE) list(
  ggplot2::scale_x_continuous(limits = pca_symmetric_limits,
    expand = ggplot2::expansion(mult = .08)),
  ggplot2::scale_y_continuous(limits = pca_symmetric_limits,
    breaks = if (two_axes) ggplot2::waiver() else NULL,
    expand = ggplot2::expansion(mult = .08)))

pca_check_axes <- function(pca, x, y = "") {
  pcs <- colnames(pca$model$rotation)
  if (!x %in% pcs) stop("Select an available principal component.")
  if (nzchar(y) && (!y %in% pcs || identical(x, y))) stop("Select two different available principal components.")
}

pca_scores_plot <- function(pca, x = "PC1", y = "PC2", color = "", mode = "auto", labels = FALSE, shape = ".Role") {
  pca_check_axes(pca, x, y)
  d <- pca_point_data(pca, color, mode, shape)
  d$.x <- d[[x]]
  d$.y <- if (nzchar(y)) d[[y]] else 0
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
    pca_origin_layers()
  p <- p + pca_point_layers(d, color, shape, size = 3, alpha = .8)
  if (labels) p <- p + ggplot2::geom_text(ggplot2::aes(label = Acquired_Sample_ID),
    check_overlap = TRUE, vjust = -0.8, size = 3)
  p <- p + ggplot2::labs(title = "PCA sample scores", subtitle = pca_plot_context(pca),
    x = pca_axis_label(pca, x, "score"), y = if (nzchar(y)) pca_axis_label(pca, y, "score") else NULL,
    caption = pca_scope_caption(pca)) + pca_scatter_theme() + pca_scatter_scales(nzchar(y))
  if (nzchar(y)) p + ggplot2::coord_equal() else p
}

pca_loadings_plot <- function(pca, x = "PC1", y = "PC2", top_n = 15L,
                              feature_color = "", feature_mode = "categorical", feature_shape = "") {
  pca_check_axes(pca, x, y)
  if (length(top_n) != 1L || !is.finite(top_n) || top_n < 0 || top_n > 100 || top_n != as.integer(top_n))
    stop("Number of loading labels must be a whole number from 0 to 100.")
  points <- pathway_point_data(pca$loadings, pca$pathway_annotation, feature_color, feature_mode, feature_shape)
  d <- points$data
  mapped <- !is.null(points$color) || !is.null(points$shape)
  d$.x <- d[[x]]
  d$.y <- if (nzchar(y)) d[[y]] else 0
  d$.weight <- d$.x^2 + d$.y^2
  selected <- order(d$.weight, decreasing = TRUE)[seq_len(min(as.integer(top_n), nrow(d)))]
  marked <- d[selected, , drop = FALSE]
  layers <- if (mapped) {
    points$data <- d
    base_layers <- pathway_point_layers(points)
    top_layer <- pathway_point_layers(points)[[1L]]
    top_layer$data <- marked
    top_layer$aes_params$size <- 3.2
    top_layer$show.legend <- FALSE
    c(base_layers, list(top_layer))
  } else list(ggplot2::geom_point(color = "grey65", size = 1.7, alpha = .7),
    ggplot2::geom_point(data = marked, color = "#176d80", size = 2))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
    pca_origin_layers() + layers +
    ggplot2::geom_text(data = marked, ggplot2::aes(label = Lipid), check_overlap = TRUE, vjust = -0.7, size = 3) +
    ggplot2::labs(title = analysis_text("PCA lipid loadings", pca), subtitle = pca_plot_context(pca),
      x = pca_axis_label(pca, x, "loading coefficient"), y = if (nzchar(y)) pca_axis_label(pca, y, "loading coefficient") else NULL,
      caption = "Loadings are prcomp rotation coefficients, not correlations or p-values.\nHighlighted by sum of squared loadings on the displayed axes; overlapping labels may be omitted.") +
    pca_scatter_theme() + pca_scatter_scales(nzchar(y)) + pathway_legend_theme(points$color)
  if (nzchar(y)) p + ggplot2::coord_equal() else p
}

# One display scale for both axes, based only on fitted samples and the PC pair.
pca_biplot_scale <- function(pca, x, y, multiplier = 1) {
  pcs <- colnames(pca$model$rotation)
  if (length(pcs) < 2L) stop("Biplot requires two nonzero principal components")
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% pcs ||
      !is.character(y) || length(y) != 1L || is.na(y) || !y %in% pcs || identical(x, y))
    stop("Select two different available principal components for the biplot.")
  if (!is.numeric(multiplier) || length(multiplier) != 1L || !is.finite(multiplier) || multiplier <= 0)
    stop("Arrow display multiplier must be one finite positive number.")
  ids <- rownames(pca$analysis_matrix)
  at <- match(ids, pca$scores$Acquired_Sample_ID)
  if (!length(at) || anyNA(at)) stop("Biplot cannot identify the fitted samples.")
  fitted_scores <- as.matrix(pca$scores[at, c(x, y), drop = FALSE])
  rotation <- pca$model$rotation[, c(x, y), drop = FALSE]
  if (!all(is.finite(fitted_scores)) || !nrow(rotation) || !all(is.finite(rotation)))
    stop("Biplot requires finite fitted-sample scores and rotation coefficients.")
  score_extent <- apply(abs(fitted_scores), 2L, max)
  loading_extent <- apply(abs(rotation), 2L, max)
  if (any(score_extent <= 0) || any(loading_extent <= 0))
    stop("Biplot cannot scale a component with zero fitted-score or loading extent.")
  automatic <- 0.75 * min(score_extent / loading_extent)
  effective <- automatic * multiplier
  if (!is.finite(automatic) || automatic <= 0 || !is.finite(effective) || effective <= 0)
    stop("Biplot arrow scale must be finite and positive; adjust the display multiplier or PC pair.")
  list(x = x, y = y, user_multiplier = multiplier, c_auto = automatic, effective_c = effective)
}

prepare_pca_biplot <- function(pca, x = "PC1", y = "PC2", top_n = 15L,
                               features = character(), multiplier = 1, labels = FALSE,
                               color = "", mode = "auto", scale = NULL, shape = ".Role",
                               feature_color = "", feature_mode = "categorical") {
  if (!is.numeric(top_n) || length(top_n) != 1L || !is.finite(top_n) ||
      top_n < 0 || top_n > 100 || top_n != trunc(top_n))
    stop("Top features to display must be a whole number from 0 to 100.")
  if (is.null(scale)) scale <- pca_biplot_scale(pca, x, y, multiplier)
  if (!identical(scale$x, x) || !identical(scale$y, y) ||
      !identical(scale$user_multiplier, multiplier)) stop("Biplot scale does not match the requested view.")
  d <- pca_point_data(pca, color, mode, shape)
  d$.x <- d[[x]]; d$.y <- d[[y]]
  if (!all(is.finite(d$.x)) || !all(is.finite(d$.y))) stop("Biplot requires finite sample scores.")
  rotation <- pca$model$rotation
  ids <- rownames(rotation)
  # Exact membership: never trim or normalize feature identifiers.
  manual <- unique(as.character(features))
  manual <- manual[!is.na(manual) & manual %in% ids]
  weight <- rotation[, x]^2 + rotation[, y]^2
  ranked <- order(-weight, seq_along(ids))
  automatic <- ranked[seq_len(min(as.integer(top_n), length(ids)))]
  selected <- ranked[ranked %in% automatic | ids[ranked] %in% manual]
  arrows <- data.frame(Lipid = ids[selected], PC_x = rep(x, length(selected)), PC_y = rep(y, length(selected)),
    Raw_x = unname(rotation[selected, x]), Raw_y = unname(rotation[selected, y]),
    Displayed_x = unname(rotation[selected, x] * scale$effective_c),
    Displayed_y = unname(rotation[selected, y] * scale$effective_c),
    Ranking_value = unname(weight[selected]), Automatic = selected %in% automatic,
    Manual = ids[selected] %in% manual, check.names = FALSE)
  if (!all(is.finite(arrows$Displayed_x)) || !all(is.finite(arrows$Displayed_y)))
    stop("Biplot arrow endpoints are nonfinite; reduce the display multiplier.")
  settings <- list(biplot_scaling_convention = "unchanged_scores_raw_rotation_separate_loading_axes",
    biplot_x = x, biplot_y = y, biplot_top_n = as.integer(top_n),
    biplot_multiplier = multiplier, biplot_c_auto = scale$c_auto,
    biplot_effective_c = scale$effective_c, biplot_labels = isTRUE(labels),
    biplot_feature_color = feature_color, biplot_feature_mode = feature_mode)
  list(samples = d, arrows = arrows, settings = settings, manual_features = manual,
    pathway_annotation = pca$pathway_annotation,
    color = color, mode = mode, shape = shape, scope = pca_scope_caption(pca), context = pca_plot_context(pca),
    variance = pca$variance$Variance_percent[match(c(x, y), pca$variance$PC)],
    variance_unit = if (identical(pca$settings$center, FALSE)) "sum of squares" else "variance",
    preview = identical(pca$settings$sample_scope, "Baseline preview; study samples only"),
    has_qc = nrow(pca$qc_analysis_matrix) > 0)
}

pca_biplot_plot <- function(prepared) {
  d <- prepared$samples; a <- prepared$arrows; s <- prepared$settings
  # Insert display-only line breaks without changing any identifier characters.
  a$.label <- vapply(a$Lipid, function(id) {
    lines <- character()
    while (nchar(id) > 36L) {
      spaces <- gregexpr("[[:space:]]", substr(id, 1L, 36L))[[1L]]
      end <- if (any(spaces > 0L)) utils::tail(spaces[spaces > 0L], 1L) else 36L
      lines <- c(lines, substr(id, 1L, end))
      id <- substring(id, end + 1L)
    }
    paste(c(lines, id), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
  # Capture the scalar in each secondary-axis closure, never a device/zoom range.
  multiplier <- s$biplot_effective_c
  feature_color <- if (is.null(s$biplot_feature_color)) "" else s$biplot_feature_color
  points <- pathway_point_data(a, prepared$pathway_annotation, feature_color,
    if (is.null(s$biplot_feature_mode)) "categorical" else s$biplot_feature_mode)
  arrows <- if (is.null(points$color)) list(ggplot2::geom_segment(data = a, inherit.aes = FALSE,
      ggplot2::aes(x = 0, y = 0, xend = Displayed_x, yend = Displayed_y),
      color = "grey40", alpha = .75, arrow = grid::arrow(length = grid::unit(.15, "cm")))) else
    list(pathway_arrow_layer(points$data), pathway_color_scale(points$color, "pathway_colour"))
  p <- ggplot2::ggplot(d, ggplot2::aes(.x, .y)) +
    pca_origin_layers() + arrows +
    ggplot2::geom_text(data = a, inherit.aes = FALSE,
      ggplot2::aes(x = Displayed_x, y = Displayed_y, label = .label),
      color = "grey30", check_overlap = TRUE, hjust = "inward", vjust = "inward", size = 3, lineheight = .9)
  p <- p + pca_point_layers(d, prepared$color, prepared$shape)
  if (s$biplot_labels) p <- p + ggplot2::geom_text(ggplot2::aes(label = Acquired_Sample_ID),
    check_overlap = TRUE, vjust = -0.8, size = 3)
  scope <- prepared$scope
  p + ggplot2::scale_x_continuous(limits = pca_symmetric_limits, expand = ggplot2::expansion(mult = .08),
      sec.axis = ggplot2::sec_axis(~ . / multiplier, name = paste(s$biplot_x, "loading coefficient"))) +
    ggplot2::scale_y_continuous(limits = pca_symmetric_limits, expand = ggplot2::expansion(mult = .08),
      sec.axis = ggplot2::sec_axis(~ . / multiplier, name = paste(s$biplot_y, "loading coefficient"))) +
    ggplot2::expand_limits(x = 0, y = 0) + ggplot2::coord_equal() +
    ggplot2::labs(title = "PCA biplot", subtitle = prepared$context,
      x = sprintf("%s score (%.1f%% %s)", s$biplot_x, prepared$variance[1], prepared$variance_unit),
      y = sprintf("%s score (%.1f%% %s)", s$biplot_y, prepared$variance[2], prepared$variance_unit),
      caption = paste0("Sample positions are unchanged PCA scores. Arrows show selected feature rotation coefficients\n",
        "on the top/right loading scales; common display multiplier c = ", format(multiplier, digits = 6, trim = TRUE),
        ".\n", scope, " Overlapping labels may be omitted.")) + pca_scatter_theme() + pathway_legend_theme(points$color)
}

pca_biplot_methods <- function() c(
  "Biplot sample positions are unchanged PCA scores. Arrows are raw rotation coefficients multiplied by one common positive display multiplier c; the top/right loading axes divide score-axis coordinates by c. These are separate units.",
  "Automatic c = 0.75 * min(Sx/Lx, Sy/Ly), multiplied by the user setting. Sx and Sy are maximum absolute scores among samples used to fit PCA; Lx and Ly are maximum absolute coefficients across all fitted features on those PCs. The 0.75 factor is a layout choice. Feature selection, projected QCs, color, shape, resizing and zoom do not change c. Including QCs in the fit can change c.",
  "Top-N is a display ranking by the sum of squared rotation coefficients on the selected PCs, with ties in fitted-feature order. Additional selections are included even outside the top-N. Larger absolute coefficients indicate larger weights in these PCs under the chosen preprocessing, not proven biological importance.",
  "For an above-center feature value, a positive coefficient contributes positively to that PC score; a negative coefficient contributes in the opposite direction. Coefficients are not feature-PC correlations, effect sizes or significance measures. Angles between raw loading arrows should not automatically be interpreted as feature-feature correlations.",
  "For fitted sample i and feature j, the selected-PC approximation is Z_hat_ij = T_ik * V_jk + T_il * V_jl. If displayed arrow endpoints replace V, divide the inner product by c. Z is the analysis matrix after the selected adjustments.",
  "Distances from samples to arrow tips have no direct statistical interpretation. Sample distances describe only the selected two-dimensional score projection; omitted PCs may contain substantial structure. Apparent group separation is exploratory, not a hypothesis test. Imputation, normalization, transformation and scaling affect the fitted structure.")

pca_pairs_plot <- function(pca, pcs, color = "", mode = "auto", shape = ".Role") {
  pcs <- unique(pcs)
  if (length(pcs) < 2L || length(pcs) > 6L || !all(pcs %in% colnames(pca$model$rotation)))
    stop("Select two to six available PCs for the scores pairs plot.")
  scores <- pca_point_data(pca, color, mode, shape)
  cells <- expand.grid(row = pcs, column = pcs, stringsAsFactors = FALSE)
  off <- cells[cells$row != cells$column, , drop = FALSE]
  d <- do.call(rbind, lapply(seq_len(nrow(off)), function(i) {
    z <- data.frame(Acquired_Sample_ID = scores$Acquired_Sample_ID,
      .x = scores[[off$column[i]]], .y = scores[[off$row[i]]],
      .row = off$row[i], .column = off$column[i])
    if (nzchar(color)) z$.color <- scores$.color
    if (nzchar(shape)) z$.shape <- scores$.shape
    z
  }))
  diagonal <- data.frame(.x = vapply(pcs, function(pc) mean(range(scores[[pc]])), numeric(1)),
    .y = vapply(pcs, function(pc) mean(range(scores[[pc]])), numeric(1)),
    .row = pcs, .column = pcs, .label = vapply(pcs, function(pc) pca_axis_label(pca, pc), character(1)))
  for (key in c(".row", ".column")) {
    d[[key]] <- factor(d[[key]], levels = pcs)
    diagonal[[key]] <- factor(diagonal[[key]], levels = pcs)
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y))
  p <- p + pca_point_layers(d, color, shape, size = 1.7, alpha = .8)
  p + ggplot2::geom_text(data = diagonal, ggplot2::aes(label = .label), size = 3.5) +
    ggplot2::facet_grid(.row ~ .column, scales = "free", drop = FALSE) +
    ggplot2::labs(title = "PCA scores pairs plot", subtitle = pca_plot_context(pca), x = NULL, y = NULL,
      caption = paste("Columns specify x PCs; rows specify y PCs. Diagonal labels show component percentages under the selected centering/scaling. No correlation tests are performed.",
        pca_scope_caption(pca), sep = "\n")) + pca_theme()
}

pca_scree_plot <- function(pca) {
  d <- pca$variance[pca$variance$Available_for_plot, , drop = FALSE]
  d$Component <- seq_len(nrow(d))
  ggplot2::ggplot(d, ggplot2::aes(x = Component, y = Variance_percent)) +
    ggplot2::geom_col(fill = "#176d80", width = .7) +
    ggplot2::geom_line(ggplot2::aes(y = Cumulative_percent), color = "#b07a16", group = 1) +
    ggplot2::geom_point(ggplot2::aes(y = Cumulative_percent), color = "#b07a16") +
    ggplot2::labs(title = paste("PCA", tolower(pca_variance_label(pca))), subtitle = pca_plot_context(pca),
      x = "Principal component", y = paste0(pca_variance_label(pca), " (%)"),
      caption = "Bars: individual components. Gold line: cumulative percentage. Denominator includes the full singular-value spectrum.") +
    ggplot2::scale_y_continuous(limits = c(0, 100.000001)) + pca_theme()
}

pca_scores_export <- function(pca) {
  out <- pca$scores
  meta <- pca$metadata[-1L]
  if (ncol(meta)) {
    # Prefix prevents metadata named PC1 from overwriting a computed score.
    names(meta) <- paste0("metadata__", names(meta))
    out <- cbind(out, meta)
  }
  out
}

pca_loadings_export <- function(pca) {
  export_feature_table(pathway_export_features(pca$loadings, pca$pathway_annotation), pca, "Lipid")
}

write_pca_bundle <- function(pca, directory, view = NULL, zoom = list()) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (folder in c("tables", "plots", "model"))
    dir.create(file.path(directory, folder), showWarnings = FALSE)
  write_table <- function(x, name) utils::write.csv(x, file.path(directory, "tables", name), row.names = FALSE, na = "")
  write_table(pca_scores_export(pca), "PCA_scores.csv")
  write_table(pca_loadings_export(pca), "PCA_loadings.csv")
  annotation_tables <- pathway_tables(pca$pathway_annotation, pca)
  for (name in names(annotation_tables)) write_table(annotation_tables[[name]], paste0(name, ".csv"))
  write_table(pca$variance, "PCA_explained_variance.csv")
  write_table(export_feature_table(pca$preprocessing, pca, "Lipid"), "PCA_feature_preprocessing.csv")
  write_table(export_feature_table(pca$removed_features, pca, "Lipid"), "PCA_omitted_features.csv")
  write_table(pca$metadata, "PCA_sample_metadata.csv")
  if (!is.null(pca$qc_eligibility)) write_table(pca$qc_eligibility, "QC_eligibility.csv")
  as_settings <- function(x) data.frame(Setting = names(x), Value = vapply(x,
    function(v) if (is.null(v)) "auto" else paste(v, collapse = "; "), character(1)))
  write_table(as_settings(pca$settings), "PCA_settings.csv")
  if (!is.null(pca$processing_settings)) write_table(as_settings(pca$processing_settings), "processing_settings.csv")
  if (!is.null(pca$provenance)) write_table(pca$provenance, "input_provenance.csv")
  write_table(data.frame(Acquired_Sample_ID = rownames(pca$analysis_matrix),
    pca$analysis_matrix, check.names = FALSE, row.names = NULL), "PCA_analysis_matrix.csv")
  write_table(data.frame(Acquired_Sample_ID = as.character(rownames(pca$qc_analysis_matrix)),
    pca$qc_analysis_matrix, check.names = FALSE, row.names = NULL), "QC_projected_analysis_matrix.csv")
  saveRDS(pca, file.path(directory, "model", "PCA_model_and_data.rds"), version = 3)
  pcs <- pca_selectable_components(pca)
  defaults <- c(pca_selected_axes(pca), list(
    color = "", mode = "categorical", shape = ".Role", labels = FALSE, top_n = 15L, pairs = utils::head(pcs, 4L),
    biplot_top_n = 15L, biplot_features = character(), biplot_multiplier = 1, biplot_labels = FALSE,
    feature_color = "", feature_mode = "categorical", feature_shape = "",
    biplot_feature_color = "", biplot_feature_mode = "categorical"))
  view <- utils::modifyList(defaults, if (is.null(view)) list() else view)
  axes <- pca_selected_axes(pca, view$x, view$y)
  pairs <- utils::head(intersect(view$pairs, pcs), 6L)
  if (!identical(axes, view[c("x", "y")])) zoom[c("scores", "loadings", "biplot")] <- list(NULL, NULL, NULL)
  if (!identical(pairs, view$pairs)) zoom$pairs <- NULL
  view[c("x", "y")] <- axes
  view$pairs <- pairs
  # One prepared snapshot supplies both presentation values and the PDF.
  biplot <- tryCatch({
    pca_require_plot_components(pca, "biplot")
    prepare_pca_biplot(pca, view$x, view$y, view$biplot_top_n,
    view$biplot_features, view$biplot_multiplier, view$biplot_labels, view$color, view$mode, shape = view$shape,
    feature_color = view$biplot_feature_color, feature_mode = view$biplot_feature_mode)
  }, error = identity)
  manual <- unique(as.character(view$biplot_features))
  manual <- manual[!is.na(manual) & manual %in% rownames(pca$model$rotation)]
  write_table(export_feature_table(data.frame(Selection_order = seq_along(manual), Lipid = manual,
    check.names = FALSE), pca, "Lipid"), "PCA_biplot_manual_features.csv")
  if (inherits(biplot, "error")) {
    biplot_settings <- list(biplot_scaling_convention = "unchanged_scores_raw_rotation_separate_loading_axes",
      biplot_x = view$x, biplot_y = view$y, biplot_top_n = view$biplot_top_n,
      biplot_multiplier = view$biplot_multiplier, biplot_c_auto = "", biplot_effective_c = "",
      biplot_labels = view$biplot_labels, biplot_status = conditionMessage(biplot))
    # A reused destination must not retain a previous successful biplot.
    unlink(file.path(directory, c("plots/PCA_biplot.pdf", "tables/PCA_biplot_arrows.csv")))
    writeLines(conditionMessage(biplot), file.path(directory, "PCA_biplot_status.txt"))
  } else {
    biplot_settings <- c(biplot$settings, list(biplot_status = "Available"))
    write_table(export_feature_table(pathway_export_features(biplot$arrows, pca$pathway_annotation), pca, "Lipid"), "PCA_biplot_arrows.csv")
    unlink(file.path(directory, "PCA_biplot_status.txt"))
  }
  # Keep exact manual IDs in their own CSV, never a delimiter-joined setting.
  biplot_settings$biplot_manual_features_file <- "tables/PCA_biplot_manual_features.csv"
  biplot_settings <- lapply(biplot_settings, function(v) if (is.numeric(v)) sprintf("%.17g", v) else v)
  zoom_settings <- unlist(lapply(c("scores", "loadings", "biplot", "pairs", "scree"), function(kind) {
    z <- zoom[[kind]]
    limits <- if (is.null(z)) rep("", 4) else if (kind == "biplot") sprintf("%.17g", c(z$xlim, z$ylim)) else c(z$xlim, z$ylim)
    stats::setNames(c(limits,
      paste(names(z$facets), unlist(z$facets), sep = "=", collapse = "; ")),
      paste0(kind, "_zoom_", c("xmin", "xmax", "ymin", "ymax", "facets")))
  }))
  write_table(as_settings(c(view[!grepl("^biplot_", names(view))], biplot_settings, as.list(zoom_settings))), "PCA_plot_settings.csv")
  plots <- list(scree = pca_scree_plot(pca))
  if (length(pcs)) {
    plots$scores <- pca_scores_plot(pca, view$x, view$y, view$color, view$mode, view$labels, shape = view$shape)
    plots$loadings <- pca_loadings_plot(pca, view$x, view$y, view$top_n, view$feature_color, view$feature_mode, view$feature_shape)
    unlink(file.path(directory, paste0("PCA_", c("scores", "loadings"), "_status.txt")))
  } else {
    unlink(file.path(directory, "plots", paste0("PCA_", c("scores", "loadings"), ".pdf")))
    for (kind in c("scores", "loadings")) writeLines(
      "Plot omitted: no PCs meet the 5% cutoff. Full tables and the fitted model are retained.",
      file.path(directory, paste0("PCA_", kind, "_status.txt")))
  }
  if (!inherits(biplot, "error")) plots$biplot <- pca_biplot_plot(biplot)
  if (length(view$pairs) >= 2L) {
    plots$pairs <- pca_pairs_plot(pca, view$pairs, view$color, view$mode, shape = view$shape)
    unlink(file.path(directory, "PCA_pairs_status.txt"))
  } else {
    unlink(file.path(directory, "plots", "PCA_pairs.pdf"))
    writeLines("Pairs plot omitted: fewer than two selected PCs meet the 5% cutoff.", file.path(directory, "PCA_pairs_status.txt"))
  }
  for (name in names(plots)) save_pca_pdf(file.path(directory, "plots", paste0("PCA_", name, ".pdf")),
    plot = if (is.null(zoom[[name]])) plots[[name]] else comparison_zoom_plot(plots[[name]], zoom[[name]]),
    width = if (name == "pairs") 12 else 10,
    height = if (name == "pairs") 11 else 8)
  writeLines(c(paste(analysis_terms(pca)$label, "exploratory PCA"), "",
    "Folder guide: tables/ contains CSV results and settings; plots/ contains PDF figures; model/ contains the saved R model.", "",
    "PDFs retain each PCA tab figure's current zoom. tables/PCA_plot_settings.csv records the limits and focused pairs cell.",
    "Zoom changes the visible window only; tables and the saved model contain the complete fitted results.", "",
    "Scores, loadings and biplot full views use dashed zero lines and ranges symmetric about zero. Zoom may move the origin outside the visible window; Reset zoom restores it.",
    "Scores and loadings share PC directions and explained-variance labels but use different numerical units. Axis centering changes the display only, not PCA preprocessing or results.",
    "Samples are rows; features are columns. Blanks are excluded.",
    paste("PCA sample scope:", pca$settings$sample_scope),
    "PCA_analysis_matrix.csv contains all fitted rows. QC_projected_analysis_matrix.csv contains only supplementary projected QCs (empty in exclude/include modes).",
    if (identical(pca$settings$processing_mode, "skip"))
      "Processing skipped. Supplied measurements and feature identifiers were retained; no filtering or imputation was performed." else
      "Source data are already filtered and imputed. The selected normalization is recorded in settings.",
    "Only the selected transformation, centering and scaling are applied. No extra imputation or pseudocount is applied.",
    paste("Component percentage basis:", pca_variance_label(pca), ". Legacy CSV percentage column names are retained; see variance_basis in PCA_settings.csv."),
    "No centering/scaling (as_is) preserves every input feature, including constant and zero columns. Center-only (none) still subtracts feature means.",
    "Center: subtract each feature mean. Unit scaling: divide by sample SD. Pareto: divide by sqrt(sample SD).",
    "Constant or numerically constant features are recorded separately. Null PCs are unavailable for plots.",
    "PCA uses stats::prcomp (SVD). Scores = analysis matrix %*% rotation. Loadings = rotation coefficients.",
    "", "Biplot convention and interpretation:", pca_biplot_methods(),
    "tables/PCA_biplot_arrows.csv records every displayed feature, raw coefficients, displayed endpoints, ranking and selection flags, including arrows outside the zoom window.",
    "tables/PCA_plot_settings.csv records the biplot PC pair, automatic and effective multipliers, user settings and zoom. Manual IDs are preserved individually in tables/PCA_biplot_manual_features.csv.",
    "PCA_scores.csv and PCA_loadings.csv retain the original analytical values. Overlapping plot labels may be omitted; all selected arrows remain in the arrow table. An unavailable biplot is explained in PCA_biplot_status.txt.", "",
    "The sign of each PC is oriented to make its largest absolute loading positive; signs are otherwise arbitrary.",
    "Variance percentages use all squared singular values, including numerically omitted PCs.",
    "Selected plot components must each contribute at least 5% (using full-precision percentages). Contributions, numerical tables and the saved model retain the complete fitted results.",
    "PCA is an exploratory linear decomposition, with no normality assumption required for its computation.",
    "Scaling, imputation, outliers and repeated samples influence the fit. Metadata color/shape mappings are display-only.",
    "Numeric shape fields are treated as categories (maximum 12). Missing shape metadata is drawn as a cross; missing color metadata is grey.",
    "Visual separation is not evidence of statistical significance or proof of normalization quality.",
    "Fits from different workflows have different PCA bases; corresponding axes need not represent the same direction.",
    "tables/PCA_analysis_matrix.csv is the exact matrix fitted after the selected adjustments (any omitted features are listed).",
    "model/PCA_model_and_data.rds contains the prcomp model and captured data/settings; no Shiny upload paths are saved.",
    "For prediction, apply the same upstream processing and selected transformation, then predict(saved$model, newdata).",
    "Do not center/scale again before predict; prcomp applies the saved center/scale internally.",
    "A model intended for prediction requires training-only fitting of preprocessing and independent validation.",
    "The HTML report includes the committed-workflow overview; full PCA artifacts are provided here.",
    "", "References:",
    "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html",
    "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/biplot.princomp.html",
    "https://doi.org/10.1186/1471-2164-7-142", "", "R session:", pca$session_info),
    file.path(directory, "PCA_README.txt"))
  invisible(directory)
}

# Native Shiny plot coordinates account for facets, resizing and display density.
pca_hover_tooltip <- function(plot, hover) {
  if (is.null(hover) || is.null(plot) || !is.data.frame(plot$data) ||
      !all(c("Acquired_Sample_ID", ".x", ".y") %in% names(plot$data))) return(NULL)
  pixels <- unlist(hover$coords_css[c("x", "y")], use.names = FALSE)
  if (!is.numeric(pixels) || length(pixels) != 2L || any(!is.finite(pixels))) return(NULL)
  near <- shiny::nearPoints(plot$data, hover, xvar = ".x", yvar = ".y", threshold = 8, maxpoints = 1)
  if (!nrow(near)) return(NULL)
  # Keep long IDs inside the plot and put the label above low-lying points.
  above <- is.numeric(hover$range$bottom) && is.numeric(hover$img_css_ratio$y) &&
    pixels[2] > mean(c(hover$range$top, hover$range$bottom)) / hover$img_css_ratio$y
  style <- sprintf("left:clamp(0px, %.1fpx, max(0px, calc(100%% - 290px)));top:%.1fpx;%s",
    pixels[1] + 12, pixels[2] + if (above) -12 else 12,
    if (above) "transform:translateY(-100%);" else "")
  shiny::div(class = "pca-hover-label", role = "tooltip", style = style,
    shiny::strong("Sample ID: "), shiny::span(as.character(near$Acquired_Sample_ID[1])))
}

pca_ui <- function(id) {
  ns <- shiny::NS(id)
  zoom_controls <- function(kind) shiny::div(class = "download-row",
    shiny::actionButton(ns(paste0(kind, "_reset")), "Reset zoom"))
  figure <- function(output_id, kind, height, hover = NULL) htmltools::tagAppendAttributes(legend_figure_ui(htmltools::tagAppendAttributes(shiny::plotOutput(ns(output_id), height = height,
    brush = shiny::brushOpts(ns(paste0(kind, "_brush")), resetOnNew = TRUE),
    dblclick = ns(paste0(kind, "_dblclick")), hover = hover),
    `data-zoom-complete` = ns(paste0(kind, "_brush_end"))), ns(paste0(kind, "_legend_ui")), height), class = "pca-figure")
  shiny::tagList(
    shiny::p("Prepared data: choose Skip processing in the sidebar, review inputs, then Load prepared data. PCA and downloads use the sidebar adjustments immediately; all default to None. With processing enabled, the baseline preview uses log2 and Pareto; commit a comparison workflow for final results."),
    shiny::uiOutput(ns("status")),
    pathway_ui(ns("pathways")),
    shiny::selectInput(ns("qc_mode"), "QC handling in final PCA", choices = pca_qc_choices, selected = "project", width = "100%"),
    shiny::helpText("Study samples only removes QCs from PCA outputs. Projection shows eligible QCs without changing study centers, scales, loadings or explained variance. Fitting study + QCs recalculates these using both and can change sample structure. These choices do not rerun filtering, imputation, normalization or QC eligibility. The baseline preview always uses study samples only."),
    shiny::checkboxInput(ns("wide"), "Use full width for figures", TRUE),
    shiny::fluidRow(
      shiny::column(2, shiny::selectInput(ns("x"), "X component", choices = character())),
      shiny::column(2, shiny::selectInput(ns("y"), "Y component", choices = character())),
      shiny::column(3, shiny::selectInput(ns("color"), "Color scores by metadata", choices = c("None" = ""))),
      shiny::column(2, shiny::selectInput(ns("color_mode"), "Metadata coloring", c("Automatic" = "auto", "Categorical" = "categorical", "Continuous numeric" = "continuous"))),
      shiny::column(3, shiny::selectInput(ns("shape"), "Shape scores by metadata", choices = c("None" = "", "Sample role" = ".Role"), selected = ".Role"))),
    shiny::helpText("Component selectors include only PCs contributing at least 5% of total variance, or uncentered sums of squares when centering is off. The cutoff uses unrounded percentages. Component contributions and numerical tables retain all fitted PCs."),
    shiny::helpText("Choose color and shape independently for Scores, Biplot and Pairs. All metadata fields except the matching ID are available, including names with spaces. Automatic color uses a gradient for numeric values; choose Categorical for numeric group codes or batch labels. Shape always treats values as categories (maximum 12; a few categories are easier to distinguish). Missing color is grey; missing shape is a cross. Neither mapping changes the fit."),
    shiny::helpText("Hover over a point in Scores, Biplot or Pairs to see its sample ID."),
    shiny::helpText("Dashed lines mark zero in Scores, Loadings and Biplot. Their full views are centered on the origin. Scores and loadings use the same selected PCs but different numerical units; compare direction and sign, not numerical distances between the plots. Percentages describe centered variance, or uncentered sums of squares when centering is off. Loadings are shown as points; the Biplot shows arrows."),
    shiny::helpText("Drag a box to zoom. Double-click or click Reset zoom to restore the full view. Each figure keeps its own zoom; in Pairs, zoom focuses the selected PC pair. PDF downloads include the current zoom."),
    shiny::tabsetPanel(id = ns("plot_tab"),
      shiny::tabPanel("Scores", value = "scores", shiny::checkboxInput(ns("labels"), "Label samples (overlapping labels may be omitted)", FALSE),
        shiny::div(class = "pca-hover-plot",
          figure("scores", "scores", "850px",
            hover = shiny::hoverOpts(ns("scores_hover"), delay = 100, nullOutside = TRUE)),
          shiny::uiOutput(ns("scores_tooltip"))), zoom_controls("scores")),
      shiny::tabPanel("Loadings", value = "loadings",
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("feature_color"), "Color loadings by pathway", c("None" = ""))),
          shiny::column(4, shiny::selectInput(ns("feature_mode"), "Feature coloring", c("Categorical" = "categorical", "Automatic" = "auto", "Continuous numeric" = "continuous"))),
          shiny::column(4, shiny::selectInput(ns("feature_shape"), "Shape loadings by pathway", c("None" = "")))),
        shiny::helpText("Upload optional pathway annotations above to enable these fields. Shape supports at most 12 categories; condensed categories work well. Missing color is grey; missing shape is a cross. Top features retain their category colors and use larger points."),
        shiny::numericInput(ns("top_n"), "Highlight / label top features on selected axes", 15, min = 0, max = 100, step = 1),
        figure("loadings", "loadings", "850px"), zoom_controls("loadings")),
      shiny::tabPanel("Biplot", value = "biplot",
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("biplot_feature_color"), "Color feature arrows by pathway", c("None" = ""))),
          shiny::column(6, shiny::selectInput(ns("biplot_feature_mode"), "Arrow coloring", c("Categorical" = "categorical", "Automatic" = "auto", "Continuous numeric" = "continuous")))),
        shiny::helpText("Feature arrow colors have their own legend. Sample points use the metadata color and shape controls above."),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("biplot_top_n"), "Top features to display", 15, min = 0, max = 100, step = 1)),
          shiny::column(6, shiny::selectizeInput(ns("biplot_features"), "Additional features", choices = character(), multiple = TRUE)),
          shiny::column(3, shiny::numericInput(ns("biplot_multiplier"), "Arrow display multiplier", 1, min = 0, step = .1))),
        shiny::checkboxInput(ns("biplot_labels"), "Label samples (overlapping labels may be omitted)", FALSE),
        shiny::helpText("Top features are ranked by the sum of squared rotation coefficients on the selected PCs. This is a display ranking. Additional selections are always included; choose 0 for manual-only arrows. The positive multiplier expands all arrows equally relative to automatic scaling."),
        shiny::div(class = "pca-hover-plot",
          figure("biplot", "biplot", "850px",
            hover = shiny::hoverOpts(ns("biplot_hover"), delay = 100, nullOutside = TRUE)),
          shiny::uiOutput(ns("biplot_tooltip"))), zoom_controls("biplot")),
      shiny::tabPanel("Pairs", value = "pairs", shiny::selectizeInput(ns("pairs"), "Components for scores pairs plot (2 to 6)", choices = character(),
        multiple = TRUE, options = list(maxItems = 6)),
        shiny::div(class = "pca-hover-plot",
          figure("pairs_plot", "pairs", "1100px",
            hover = shiny::hoverOpts(ns("pairs_hover"), delay = 100, nullOutside = TRUE)),
          shiny::uiOutput(ns("pairs_tooltip"))), zoom_controls("pairs")),
      shiny::tabPanel("Component contributions", value = "scree", figure("scree", "scree", "750px"), zoom_controls("scree"),
        shiny::tableOutput(ns("variance"))),
      shiny::tabPanel("Tables", value = "tables", shiny::h4("Sample scores and metadata"), table_output(ns("scores_table")),
        shiny::uiOutput(ns("loadings_heading")), table_output(ns("loadings_table")),
        shiny::h4("Features omitted from PCA"), table_output(ns("removed_table")))),
    shiny::uiOutput(ns("downloads")),
    shiny::helpText("Prepared-data downloads are available directly. With processing enabled, commit a comparison workflow first. QC handling applies immediately to final tables, figures and reports. Blanks are excluded. Prepared QCs require complete measurements and receive no imputation."),
    shiny::tags$details(shiny::tags$summary("PCA methods and interpretation"),
      shiny::p("PCA uses stats::prcomp, a singular value decomposition of the sample-by-feature matrix. It is a linear, exploratory decomposition; no normality assumption is required to compute it, and no hypothesis tests or confidence ellipses are added."),
      shiny::p("When selected, log2 precedes any centering or scaling. Unit-variance scaling divides by the feature sample standard deviation (SD); Pareto divides by the square root of that SD. Center-only subtracts means without rescaling. None — no centering or scaling preserves the selected values exactly. Pareto is a starting option, not a universal optimum; inspect sensitivity to scaling and imputation."),
      shiny::p("Centered fits omit constant or numerically constant features and list them. No centering or scaling retains all features; its component percentages describe uncentered sums of squares, not centered variance. Loadings are rotation coefficients, not feature-PC correlations, effect sizes or significance measures."),
      lapply(pca_biplot_methods(), shiny::p),
      shiny::p("Outliers, technical variation, imputation and repeated samples can drive the components. Apparent group separation is not a statistical test. Stage-specific PCA fits have different axes; compare sample structure, not PC coordinates as if they shared a basis."),
      shiny::p("The PCA ZIP and the full results ZIP include PCA CSVs, PDFs, model, analysis matrix and settings. The HTML report summarizes the selected workflow and PCA; complete PCA tables and model are included in the ZIP exports."),
      shiny::tags$a(href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html", target = "_blank", rel = "noopener", "R prcomp documentation"),
      shiny::span(" | "), shiny::tags$a(href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/biplot.princomp.html", target = "_blank", rel = "noopener", "R biplot scaling conventions"),
      shiny::span(" | "), shiny::tags$a(href = "https://doi.org/10.1186/1471-2164-7-142", target = "_blank", rel = "noopener", "van den Berg et al., 2006: scaling and transformations")))
}

apply_pca_snapshot <- function(result, snapshot) {
  if (is.null(snapshot)) return(result)
  result$selected_pca <- snapshot$fit
  result$pathway_annotation <- snapshot$pathway_annotation
  result$pca_view <- snapshot$view
  result$pca_plot_errors <- snapshot$plot_errors
  result$plots <- c(result$plots[!grepl("^pca_", names(result$plots))], snapshot$plots)
  sizes <- lapply(snapshot$plots, function(p) list(width = 12, height = 8))
  result$plot_sizes <- c(result$plot_sizes[!grepl("^pca_", names(result$plot_sizes))], sizes)
  result
}

pca_server <- function(id, current_result, base_result = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    annotation_result <- shiny::reactive({
      tryCatch(if (!is.null(base_result)) base_result() else current_result(), shiny.silent.error = function(e) NULL)
    })
    pathways <- pathway_server("pathways", annotation_result)
    shiny::observeEvent(pathways$annotation(), {
      a <- pathways$annotation()
      choices <- pathway_choices(a)
      default <- pathway_default_field(a, analysis_terms(annotation_result())$id)
      for (name in c("feature_color", "biplot_feature_color"))
        shiny::updateSelectInput(session, name, choices = choices, selected = default)
      shiny::updateSelectInput(session, "feature_shape", choices = choices, selected = "")
      for (name in c("feature_mode", "biplot_feature_mode")) shiny::updateSelectInput(session, name, selected = "categorical")
    }, ignoreNULL = FALSE)
    plot_kinds <- c("scores", "loadings", "biplot", "pairs", "scree")
    zoom <- shiny::reactiveValues(scores = NULL, loadings = NULL, biplot = NULL, pairs = NULL, scree = NULL)
    brushes <- shiny::reactiveValues(scores = NULL, loadings = NULL, biplot = NULL, pairs = NULL, scree = NULL)
    zoom_generation <- shiny::reactiveValues(scores = 0L, loadings = 0L, biplot = 0L, pairs = 0L, scree = 0L)
    clear_brush <- function(kind) {
      brushes[[kind]] <- NULL
      zoom_generation[[kind]] <- zoom_generation[[kind]] + 1L
      session$sendCustomMessage("plot-zoom-cancel", list(
        id = session$ns(paste0(kind, "_brush_end")), generation = zoom_generation[[kind]]))
      session$resetBrush(session$ns(paste0(kind, "_brush")))
    }
    clear_zoom <- function(kind) {
      zoom[[kind]] <- NULL
      clear_brush(kind)
    }
    # QC membership changes the fit; color, shape, axes and labels never refit it.
    fit_state <- shiny::reactive({
      r <- tryCatch(current_result(), shiny.silent.error = function(e) NULL)
      preview <- is.null(r)
      if (preview && !is.null(base_result))
        r <- tryCatch(base_result(), shiny.silent.error = function(e) NULL)
      if (is.null(r)) return(list(fit = NULL, preview = preview, error = if (is.null(base_result))
        "Commit a workflow in Compare preprocessing to see PCA." else "Run processing or load prepared data to see PCA."))
      if (preview && identical(r$settings$processing_mode, "skip"))
        return(list(fit = NULL, preview = TRUE, error = "Choose valid direct PCA adjustments to view prepared data."))
      tryCatch({
        if (preview) {
          # Use the shared baseline, never the legacy TSN values or an uncommitted choice.
          r$selection <- NULL
          r$qc_imputed <- NULL
          p <- fit_lipid_pca(r, "imputed", "log2", "pareto", qc_mode = "exclude")
          p$settings$normalization <- "none"
          p$settings$sample_scope <- "Baseline preview; study samples only"
        } else {
          qc_mode <- if (is.null(input$qc_mode)) "project" else input$qc_mode
          p <- fit_lipid_pca(r, "tsn", r$selection$transform, r$selection$scaling, qc_mode = qc_mode)
        }
        list(fit = p, error = NULL, preview = preview)
      }, error = function(e) list(fit = NULL, error = conditionMessage(e), preview = preview))
    })
    fit <- shiny::reactive({
      state <- fit_state()
      shiny::validate(shiny::need(is.null(state$error), state$error))
      state$fit
    })
    display_fit <- shiny::reactive({
      p <- fit()
      p$pathway_annotation <- pathways$annotation()
      p
    })
    shiny::observeEvent(fit_state(), {
      state <- fit_state()
      if (is.null(state$fit)) {
        shiny::updateNumericInput(session, "top_n", label = "Highlight / label top features on selected axes")
        shiny::updateNumericInput(session, "biplot_top_n", label = "Top features to display")
        shiny::updateSelectizeInput(session, "biplot_features", label = "Additional features", choices = character(), selected = character(), server = TRUE)
        for (name in c("x", "y")) shiny::updateSelectInput(session, name, choices = character(), selected = character())
        shiny::updateSelectizeInput(session, "pairs", choices = character(), selected = character())
        shiny::updateSelectInput(session, "color", choices = c("None" = ""), selected = "")
        shiny::updateSelectInput(session, "shape", choices = c("None" = "", "Sample role" = ".Role"), selected = ".Role")
        return()
      }
      p <- state$fit; pcs <- pca_selectable_components(p)
      shiny::updateNumericInput(session, "top_n", label = analysis_text("Highlight / label top lipids on selected axes", p))
      shiny::updateNumericInput(session, "biplot_top_n", label = analysis_text("Top lipids to display", p))
      feature_ids <- rownames(p$model$rotation)
      shiny::updateSelectizeInput(session, "biplot_features", label = analysis_text("Additional lipids", p),
        choices = stats::setNames(feature_ids, feature_ids),
        selected = intersect(shiny::isolate(input$biplot_features), feature_ids), server = TRUE)
      a <- pca_selected_axes(p, shiny::isolate(input$x), shiny::isolate(input$y))
      x <- a$x; y <- a$y
      y_choices <- setdiff(pcs, x)
      choices <- stats::setNames(pcs, vapply(pcs, function(pc) pca_axis_label(p, pc), character(1)))
      shiny::updateSelectInput(session, "x", choices = choices, selected = x)
      y_labels <- stats::setNames(y_choices, vapply(y_choices, function(pc) pca_axis_label(p, pc), character(1)))
      shiny::updateSelectInput(session, "y", choices = y_labels, selected = y)
      old_pairs <- shiny::isolate(input$pairs)
      selected <- if (is.null(old_pairs)) utils::head(pcs, 4L) else intersect(old_pairs, pcs)
      shiny::updateSelectizeInput(session, "pairs", choices = choices, selected = utils::head(selected, 6L))
      old_color <- shiny::isolate(input$color)
      selected_color <- metadata_color_selection(p$metadata, old_color, ".Role")
      shiny::updateSelectInput(session, "color", choices = metadata_color_choices(p$metadata), selected = selected_color)
      selected_shape <- metadata_color_selection(p$metadata, shiny::isolate(input$shape), ".Role")
      shiny::updateSelectInput(session, "shape", choices = metadata_color_choices(p$metadata), selected = selected_shape)
    }, ignoreNULL = TRUE)
    shiny::observeEvent(input$x, {
      p <- fit(); pcs <- pca_selectable_components(p)
      shiny::req(input$x %in% pcs)
      alternatives <- setdiff(pcs, input$x)
      y <- shiny::isolate(input$y)
      if (!length(y) || !y %in% alternatives) y <- if (length(alternatives)) alternatives[1L] else ""
      choices <- stats::setNames(alternatives, vapply(alternatives, function(pc) pca_axis_label(p, pc), character(1)))
      shiny::updateSelectInput(session, "y", choices = choices, selected = y)
    }, ignoreNULL = TRUE)
    output$status <- shiny::renderUI({
      state <- fit_state()
      if (!is.null(state$error)) return(shiny::div(class = "notice warn", state$error))
      s <- state$fit$settings
      eligible <- length(pca_selectable_components(state$fit))
      context <- if (state$preview) "Baseline preview: no additional normalization | log2 | Pareto scaling | study samples only" else
        paste(if (identical(s$processing_mode, "skip")) "Processing skipped:" else "Committed workflow:", s$normalization, "|", s$transform, "|", if (s$scaling == "as_is") "No centering or scaling" else paste(s$scaling, "scaling"))
      shiny::div(class = "notice", shiny::p(context), shiny::p(s$sample_scope),
        if (!is.null(state$fit$qc_eligibility) && any(!state$fit$qc_eligibility$Eligible))
          shiny::p(paste("Incomplete QCs excluded (no imputation):", paste(state$fit$qc_eligibility$Acquired_Sample_ID[!state$fit$qc_eligibility$Eligible], collapse = ", "))),
        paste(s$study_samples, "study samples |", s$fitted_qcs, "QCs fitted |", s$projected_qcs,
        "QCs projected |", s$excluded_qcs, "eligible QCs excluded |", s$fitted_features,
        analysis_text("fitted lipids |", s), s$omitted_constant_features, "constant features omitted |", s$available_PCs, "nonzero PCs |", eligible, "PCs meet the 5% cutoff"),
        if (!state$preview && s$eligible_qcs == 0L) shiny::p("No eligible QCs are available for this workflow; PCA uses study samples only."),
        if (eligible == 1L) shiny::p("Only one PC meets the 5% cutoff. Scores and loadings are one-dimensional; biplot and pairs require two qualifying components."),
        if (eligible == 0L) shiny::p("No PCs meet the 5% cutoff. Component contributions, tables and model downloads remain available."))
    })
    axes <- shiny::reactive(pca_selected_axes(fit(), input$x, input$y))
    view <- shiny::reactive({
      p <- fit(); pcs <- pca_selectable_components(p); a <- axes()
      color <- metadata_color_selection(p$metadata, input$color)
      # Defaults also apply if a download arrives before PC selectors initialize.
      pairs <- if (is.null(input$pairs)) utils::head(pcs, 4L) else utils::head(intersect(input$pairs, pcs), 6L)
      list(x = a$x, y = a$y, color = color, mode = if (is.null(input$color_mode)) "auto" else input$color_mode,
        shape = metadata_color_selection(p$metadata, input$shape, ".Role"),
        labels = isTRUE(input$labels), top_n = if (is.null(input$top_n)) 15 else input$top_n, pairs = pairs,
        feature_color = pathway_field(pathways$annotation(), input$feature_color,
          pathway_default_field(pathways$annotation(), analysis_terms(p)$id)),
        feature_mode = if (is.null(input$feature_mode)) "categorical" else input$feature_mode,
        feature_shape = pathway_field(pathways$annotation(), input$feature_shape))
    })
    biplot_view <- shiny::reactive(list(
      biplot_top_n = if (is.null(input$biplot_top_n)) 15L else input$biplot_top_n,
      biplot_features = intersect(input$biplot_features, rownames(fit()$model$rotation)),
      biplot_multiplier = if (is.null(input$biplot_multiplier)) 1 else input$biplot_multiplier,
      biplot_labels = isTRUE(input$biplot_labels),
      biplot_feature_color = pathway_field(pathways$annotation(), input$biplot_feature_color,
        pathway_default_field(pathways$annotation(), analysis_terms(fit())$id)),
      biplot_feature_mode = if (is.null(input$biplot_feature_mode)) "categorical" else input$biplot_feature_mode))
    export_view <- shiny::reactive(c(view(), biplot_view()))
    biplot_scale <- shiny::reactive({
      pca_require_plot_components(fit(), "biplot")
      a <- axes()
      pca_biplot_scale(fit(), a$x, a$y, if (is.null(input$biplot_multiplier)) 1 else input$biplot_multiplier)
    })
    biplot_data <- shiny::reactive({
      v <- view(); b <- biplot_view()
      prepare_pca_biplot(display_fit(), v$x, v$y, b$biplot_top_n, b$biplot_features,
        b$biplot_multiplier, b$biplot_labels, v$color, v$mode, scale = biplot_scale(), shape = v$shape,
        feature_color = b$biplot_feature_color, feature_mode = b$biplot_feature_mode)
    })
    make_plot <- function(kind) {
      p <- display_fit(); v <- view()
      tryCatch({
        pca_require_plot_components(p, kind, v$pairs)
        switch(kind,
        scores = pca_scores_plot(p, v$x, v$y, v$color, v$mode, v$labels, shape = v$shape),
        loadings = pca_loadings_plot(p, v$x, v$y, v$top_n, v$feature_color, v$feature_mode, v$feature_shape),
        biplot = pca_biplot_plot(biplot_data()),
        pairs = pca_pairs_plot(p, v$pairs, v$color, v$mode, shape = v$shape),
        scree = pca_scree_plot(p), stop("Choose a plot tab before downloading a plot."))
      },
        error = function(e) { shiny::validate(shiny::need(FALSE, conditionMessage(e))) })
    }
    draw <- function(kind) comparison_zoom_plot(make_plot(kind), zoom[[kind]])
    # Limits belong to the displayed coordinates, not the numerical PCA fit.
    shiny::observeEvent(fit_state(), {
      for (kind in plot_kinds) clear_zoom(kind)
    }, ignoreNULL = FALSE, priority = 20)
    shiny::observeEvent(list(input$x, input$y), {
      clear_zoom("scores"); clear_zoom("loadings"); clear_zoom("biplot")
    }, ignoreNULL = FALSE, priority = 20)
    shiny::observeEvent(list(input$biplot_top_n, input$biplot_features, input$biplot_multiplier),
      clear_zoom("biplot"), ignoreNULL = FALSE, priority = 20)
    shiny::observeEvent(input$pairs, clear_zoom("pairs"), ignoreNULL = FALSE, priority = 20)
    for (kind in plot_kinds) local({
      k <- kind
      # A simultaneous component/workflow change must discard a stale brush.
      shiny::observeEvent(input[[paste0(k, "_brush_end")]], {
        brushes[[k]] <- input[[paste0(k, "_brush_end")]]
      }, ignoreNULL = FALSE, priority = 30)
      shiny::observeEvent(input[[paste0(k, "_brush_end")]], {
        bounds <- plot_zoom_event_bounds(brushes[[k]], zoom_generation[[k]])
        if (is.null(bounds)) return()
        p <- make_plot(k)
        if (!is.data.frame(p$data) || !nrow(p$data)) return()
        # Once a pairs cell is focused, subsequent brushes omit facet metadata.
        if (!length(bounds$facets) && !is.null(zoom[[k]])) bounds$facets <- zoom[[k]]$facets
        zoom[[k]] <- bounds
        clear_brush(k)
      })
      shiny::observeEvent(input[[paste0(k, "_reset")]], clear_zoom(k), priority = 20)
      shiny::observeEvent(input[[paste0(k, "_dblclick")]], clear_zoom(k), priority = 20)
    })
    # Hover reads cached plot data; pointer movement never redraws or refits PCA.
    score_plot <- shiny::reactive(draw("scores"))
    biplot_plot <- shiny::reactive(draw("biplot"))
    pairs_plot <- shiny::reactive(draw("pairs"))
    hover <- shiny::reactiveValues(scores = NULL, biplot = NULL, pairs = NULL)
    shiny::observeEvent(list(fit_state(), input$x, input$y, input$pairs, input$color,
      input$color_mode, input$shape, input$labels, input$plot_tab, input$wide, zoom$scores, zoom$pairs), {
      hover$scores <- NULL; hover$pairs <- NULL
    }, ignoreNULL = FALSE, priority = 20)
    shiny::observeEvent(list(fit_state(), input$x, input$y, input$color, input$color_mode, input$shape,
      input$biplot_top_n, input$biplot_features, input$biplot_multiplier, input$biplot_labels,
      input$plot_tab, input$wide, zoom$biplot,
      session$clientData[[paste0("output_", session$ns("biplot"), "_width")]],
      session$clientData[[paste0("output_", session$ns("biplot"), "_height")]]), {
      hover$biplot <- NULL
    }, ignoreNULL = FALSE, priority = 20)
    shiny::observeEvent(input$scores_hover, { hover$scores <- input$scores_hover }, ignoreNULL = FALSE)
    shiny::observeEvent(input$pairs_hover, { hover$pairs <- input$pairs_hover }, ignoreNULL = FALSE)
    # Apply pointer events before view resets, so a simultaneous stale event loses.
    shiny::observeEvent(input$biplot_hover, { hover$biplot <- input$biplot_hover }, ignoreNULL = FALSE, priority = 30)
    output$scores_tooltip <- shiny::renderUI(pca_hover_tooltip(score_plot(), hover$scores))
    output$pairs_tooltip <- shiny::renderUI(pca_hover_tooltip(pairs_plot(), hover$pairs))
    output$biplot_tooltip <- shiny::renderUI({
      if (is.null(hover$biplot)) return(NULL)
      pca_hover_tooltip(biplot_plot(), hover$biplot)
    })
    # Return ggplot objects so Shiny exposes their data coordinate maps.
    output$scores <- shiny::renderPlot(plot_without_legend(score_plot()), res = 110)
    output$loadings <- shiny::renderPlot(plot_without_legend(draw("loadings")), res = 110)
    output$biplot <- shiny::renderPlot(plot_without_legend(biplot_plot()), res = 110)
    output$pairs_plot <- shiny::renderPlot(plot_without_legend(pairs_plot()), res = 110)
    output$scree <- shiny::renderPlot(draw("scree"), res = 110)
    for (kind in plot_kinds) local({
      k <- kind
      bind_plot_legend(output, session, k, shiny::reactive(draw(k)))
    })
    output$variance <- shiny::renderTable(pca_variance_table(fit()), digits = 3, striped = TRUE)
    widget <- function(x) DT::datatable(x, rownames = FALSE, escape = TRUE, fillContainer = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, lengthChange = FALSE))
    output$scores_table <- DT::renderDT(widget(pca_scores_export(fit())), server = TRUE)
    output$loadings_heading <- shiny::renderUI(shiny::h4(analysis_text("Lipid loadings (all fitted features)", fit())))
    output$loadings_table <- DT::renderDT(widget(pca_loadings_export(display_fit())), server = TRUE)
    output$removed_table <- DT::renderDT(widget(export_feature_table(fit()$removed_features, fit(), "Lipid")), server = TRUE)
    # Viewing a baseline never grants access to final-workflow exports.
    export_fit <- shiny::reactive({
      current_result()
      shiny::validate(shiny::need(!fit_state()$preview, "Commit a workflow before downloading PCA results."))
      display_fit()
    })
    output$downloads <- shiny::renderUI({
      state <- fit_state()
      if (is.null(state$fit) || state$preview) return(NULL)
      ns <- session$ns
      shiny::div(class = "download-row", shiny::downloadButton(ns("download_plot"), "Current plot (PDF)"),
        shiny::downloadButton(ns("download_scores"), "Scores (CSV)"), shiny::downloadButton(ns("download_loadings"), "Loadings (CSV)"),
        shiny::downloadButton(ns("download_bundle"), "PCA results + plots (ZIP)"))
    })
    output$download_scores <- shiny::downloadHandler(filename = function() metlip_download_name("PCA_scores.csv", current_result()$metlip_id), content = function(file)
      utils::write.csv(pca_scores_export(export_fit()), file, row.names = FALSE, na = ""))
    output$download_loadings <- shiny::downloadHandler(filename = function() metlip_download_name("PCA_loadings.csv", current_result()$metlip_id), content = function(file)
      utils::write.csv(pca_loadings_export(export_fit()), file, row.names = FALSE, na = ""))
    output$download_plot <- shiny::downloadHandler(filename = function() metlip_download_name(paste0("PCA_", input$plot_tab, ".pdf"), current_result()$metlip_id), content = function(file) {
      export_fit()
      plot <- draw(input$plot_tab)
      save_pca_pdf(file, plot = plot, width = if (input$plot_tab == "pairs") 12 else 10,
        height = if (input$plot_tab == "pairs") 11 else 8)
    })
    output$download_bundle <- shiny::downloadHandler(filename = function() metlip_download_name("PCA_results.zip", current_result()$metlip_id), content = function(file) {
      folder <- tempfile("pca_export_"); dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      tryCatch({
        write_pca_bundle(export_fit(), folder, export_view(), shiny::reactiveValuesToList(zoom))
        write_zip_bundle(file, folder)
      }, error = function(e) {
        shiny::showNotification(paste("PCA export failed:", conditionMessage(e)), type = "error", duration = 15)
        stop(e)
      })
    }, contentType = "application/zip")
    # Called at download time, capturing the same fit/settings as the PCA tab.
    # A PCA failure must not block the existing processing result exports.
    export_bundle <- function(directory) {
      staged <- tempfile("pca_export_")
      on.exit(unlink(staged, recursive = TRUE), add = TRUE)
      tryCatch({
        # Finish tables, model and plots before touching the complete archive's
        # PCA folder, which can already contain committed analysis tables.
        write_pca_bundle(export_fit(), staged, export_view(), shiny::reactiveValuesToList(zoom))
        for (path in list.files(staged, recursive = TRUE)) {
          target <- file.path(directory, path)
          dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
          if (!file.copy(file.path(staged, path), target, overwrite = TRUE))
            stop("Could not prepare the PCA export file: ", path)
        }
      }, error = function(e) {
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
        writeLines(c("PCA export could not be completed.", conditionMessage(e),
          "Review the PCA tab and its settings. Processing/QC exports remain valid."), file.path(directory, "PCA_STATUS.txt"))
        shiny::showNotification(paste("Processing exported; PCA unavailable:", conditionMessage(e)), type = "warning", duration = 12)
      })
    }
    # Main results and reports consume this exact fit, avoiding a stale study-only
    # model after the user changes QC handling. The comparison tab keeps its own fit.
    attr(export_bundle, "snapshot") <- shiny::reactive({
      state <- fit_state()
      if (state$preview) return(NULL)
      if (!is.null(state$error)) return(list(fit = simpleError(state$error), pathway_annotation = pathways$annotation(), view = list(), plots = list(), plot_errors = list()))
      plots <- lapply(c("scores", "loadings", "scree"), function(kind)
        tryCatch(draw(kind), error = identity))
      names(plots) <- paste0("pca_", c("scores", "loadings", "scree"))
      failed <- vapply(plots, inherits, logical(1), "error")
      list(fit = display_fit(), pathway_annotation = pathways$annotation(), view = export_view(), plots = plots[!failed],
        plot_errors = lapply(plots[failed], conditionMessage))
    })
    export_bundle
  })
}
