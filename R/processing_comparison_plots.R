comparison_theme <- function() ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom", legend.box = "vertical",
    plot.title.position = "plot", plot.caption = ggplot2::element_text(hjust = 0))

empty_plot <- function(message) ggplot2::ggplot() +
  ggplot2::annotate("text", x = 0, y = 0, label = paste(strwrap(message, 65), collapse = "\n")) + ggplot2::theme_void()

comparison_color_scale <- function(column, mode, aesthetic = "colour") {
  if (aesthetic == "fill") {
    if (!nzchar(column)) return(ggplot2::scale_fill_manual(values = "#176d80", guide = "none"))
    if (identical(column, ".Imputed_fraction") && mode == "continuous") return(imputed_fraction_scale("fill"))
    if (mode == "continuous") return(ggplot2::scale_fill_viridis_c(name = column, na.value = "grey65"))
    return(ggplot2::scale_fill_discrete(name = column, na.value = "grey65", drop = FALSE))
  }
  if (!nzchar(column)) return(ggplot2::scale_color_manual(values = "#176d80", guide = "none"))
  pca_color_scale(column, mode)
}

# Shiny supplies brush bounds in data units, including on logarithmic axes.
comparison_zoom_bounds <- function(brush) {
  if (!is.list(brush)) return(NULL)
  keys <- c("xmin", "xmax", "ymin", "ymax")
  valid <- vapply(keys, function(key) is.numeric(brush[[key]]) &&
    length(brush[[key]]) == 1L && is.finite(brush[[key]]), logical(1))
  if (!all(valid)) return(NULL)
  xlim <- c(brush$xmin, brush$xmax); ylim <- c(brush$ymin, brush$ymax)
  if (diff(xlim) <= 0 || diff(ylim) <= 0) return(NULL)
  logs <- if (is.list(brush$log)) brush$log else list()
  if ((!is.null(logs$x) && any(xlim <= 0)) ||
      (!is.null(logs$y) && any(ylim <= 0))) return(NULL)
  mapping <- if (is.list(brush$mapping)) brush$mapping else list()
  facets <- list()
  for (key in c("panelvar1", "panelvar2")) {
    column <- mapping[[key]]; value <- brush[[key]]
    if (is.character(column) && length(column) == 1L && nzchar(column) &&
        is.atomic(value) && length(value) == 1L && !is.na(value)) facets[[column]] <- value
  }
  # Diagonal pairs cells contain only a component label, not sample points.
  if (all(c(".row", ".column") %in% names(facets)) &&
      identical(as.character(facets$.row), as.character(facets$.column))) return(NULL)
  list(xlim = xlim, ylim = ylim, facets = facets)
}

# Reject completions from a coordinate map invalidated by reset/settings changes.
plot_zoom_event_bounds <- function(event, generation) {
  if (!is.list(event) || !is.numeric(event$generation) ||
      length(event$generation) != 1L || !is.finite(event$generation) ||
      event$generation != generation) return(NULL)
  comparison_zoom_bounds(event$brush)
}

comparison_zoom_plot <- function(plot, zoom) {
  if (is.null(zoom) || !is.data.frame(plot$data) || !nrow(plot$data)) return(plot)
  facets <- zoom$facets
  if (length(facets)) {
    if (!all(names(facets) %in% names(plot$data))) return(plot)
    filter_facet <- function(data) {
      if (!is.data.frame(data)) return(data)
      for (column in intersect(names(facets), names(data)))
        data <- data[!is.na(data[[column]]) & data[[column]] == facets[[column]], , drop = FALSE]
      data
    }
    data <- filter_facet(plot$data)
    if (!nrow(data)) return(plot)
    # Capture PC labels before removing the other pairs cells.
    if (all(c(".row", ".column") %in% names(facets))) {
      axis_label <- function(pc) {
        for (layer in plot$layers) {
          d <- layer$data
          if (is.data.frame(d) && all(c(".row", ".column", ".label") %in% names(d))) {
            label <- d$.label[d$.row == pc & d$.column == pc]
            if (length(label)) return(as.character(label[1]))
          }
        }
        as.character(pc)
      }
      plot <- plot + ggplot2::labs(x = axis_label(facets$.column), y = axis_label(facets$.row))
    }
    plot$data <- data
    plot$layers <- lapply(plot$layers, function(layer)
      ggplot2::ggproto(NULL, layer, data = filter_facet(layer$data)))
    plot$facet <- ggplot2::facet_null()
  }
  # Change the visible window, never the scale limits or statistical input.
  # Cloning also retains the equal-axis ratio used by PCA scores/loadings.
  coordinates <- plot$coordinates
  plot$coordinates <- ggplot2::ggproto(NULL, coordinates,
    limits = list(x = zoom$xlim, y = zoom$ylim), expand = FALSE)
  plot
}

# Keep raw scorecards intact for audit exports; simplify the app and report view.
comparison_scorecard_view <- function(comparison) {
  if (is.null(comparison)) return(NULL)
  scorecard <- comparison$scorecard
  scorecard <- scorecard[, setdiff(names(scorecard), c("Median_abs_QC_order_rho", "Median_replicate_log2_SD", "Truth_log2_RMSE")), drop = FALSE]
  if (!nzchar(comparison$config$pool) && !nzchar(comparison$config$batch))
    names(scorecard)[names(scorecard) == "Eligible_feature_strata"] <- "Eligible_features"
  rownames(scorecard) <- NULL
  scorecard
}

comparison_values <- function(comparison, method, scope = "all", observed = FALSE) {
  validate_normalization_method(method)
  x <- if (observed) comparison$evidence[[method]]$normalized_observed else comparison$candidates[[method]]$x
  ids <- if (scope == "study") comparison$input$study else if (scope == "qc") comparison$evidence[[method]]$usable_qc else
    c(comparison$input$study, comparison$evidence[[method]]$usable_qc)
  d <- matrix_long(x[ids, , drop = FALSE], "Value")
  m <- comparison$input$metadata
  # User fields can be named Value, Lipid, or Acquired_Sample_ID. Keep them in
  # the metadata table so they cannot overwrite measurements or identifiers.
  d$.Role <- m$.Role[match(d$Acquired_Sample_ID, m[[1]])]
  d$Log2 <- log2(d$Value)
  d
}

comparison_plot <- function(comparison, method, type = "density", fit = NULL, color = ".Role",
                            sample = "", feature = "", scope = "all", observed = FALSE,
                            pcx = "PC1", pcy = "PC2", color_mode = "auto") {
  validate_normalization_method(method)
  label <- normalization_labels[[method]]
  input <- comparison$input; evidence <- comparison$evidence[[method]]
  d <- comparison_values(comparison, method, scope, observed)
  ids <- unique(d$Acquired_Sample_ID)
  sample_view <- !type %in% c("loadings", "scree", "qc_rsd", "truth")
  if (sample_view && !length(ids)) return(empty_plot("No eligible samples for this view. QCs must meet the detection criteria; choose Studies to inspect study samples."))
  d$.highlight <- d$Acquired_Sample_ID == sample
  color <- metadata_color_selection(input$metadata, color, ".Role")
  sub <- paste(label, if (observed) "observed measurements only" else "shared imputation included", sep = " | ")
  if (type %in% c("scores", "pairs", "loadings", "scree")) {
    if (inherits(fit, "error")) return(empty_plot(conditionMessage(fit)))
    if (is.null(fit)) return(empty_plot("PCA is unavailable for these settings."))
    pcs <- colnames(fit$model$rotation)
    if (!pcx %in% pcs) pcx <- pcs[1]
    if (!pcy %in% setdiff(pcs, pcx)) pcy <- if (length(setdiff(pcs, pcx))) setdiff(pcs, pcx)[1] else ""
    if (type == "scree") return(pca_scree_plot(fit))
    if (type == "loadings") {
      p <- pca_loadings_plot(fit, pcx, pcy, 12)
      if (nzchar(feature) && feature %in% fit$loadings$Lipid) {
        z <- fit$loadings[fit$loadings$Lipid == feature, , drop = FALSE]
        p <- p + ggplot2::annotate("point", x = z[[pcx]], y = if (nzchar(pcy)) z[[pcy]] else 0,
                                  color = "#d95f02", size = 5, shape = 1)
      }
      return(p)
    }
    cf <- metadata_color_selection(fit$metadata, color, ".Role")
    # Resolve automatic types before filtering: QC-only views can have no
    # metadata even when the full field is numeric for the study samples.
    spec <- tryCatch(metadata_color_spec(fit$metadata, cf, color_mode), error = identity)
    if (inherits(spec, "error")) return(empty_plot(conditionMessage(spec)))
    color_mode <- spec$mode
    if (nzchar(cf)) fit$metadata[[cf]] <- spec$values
    z <- pca_color_data(fit, cf, color_mode)
    z <- z[z$Acquired_Sample_ID %in% ids, , drop = FALSE]
    # Filter only the displayed scores and aligned metadata. The study-fitted
    # model, loadings, variance and QC projection coordinates stay unchanged.
    fit$scores <- fit$scores[fit$scores$Acquired_Sample_ID %in% ids, , drop = FALSE]
    fit$metadata <- fit$metadata[match(fit$scores$Acquired_Sample_ID, fit$metadata[[1]]), , drop = FALSE]
    if (!nrow(fit$scores)) return(empty_plot("No eligible samples for this PCA view."))
    if (type == "pairs") {
      if (length(pcs) < 2) return(empty_plot("Fewer than two nonzero PCs; pairs plot unavailable."))
      return(pca_pairs_plot(fit, utils::head(pcs, 4), cf, color_mode))
    }
    z$.x <- z[[pcx]]; z$.y <- if (nzchar(pcy)) z[[pcy]] else 0
    z$.highlight <- z$Acquired_Sample_ID == sample
    p <- ggplot2::ggplot(z, ggplot2::aes(.x, .y, shape = .role)) + pca_origin_layers()
    if (nzchar(cf)) p <- p + ggplot2::geom_point(ggplot2::aes(color = .color), size = 2.7, alpha = .85) +
      pca_color_scale(cf, color_mode) else p <- p + ggplot2::geom_point(size = 2.7, alpha = .85, color = "#176d80")
    p <- p +
      ggplot2::geom_point(data = z[z$.highlight, , drop = FALSE], size = 5, stroke = 1.5, shape = 1, color = "#d95f02") +
      ggplot2::labs(title = "PCA scores", subtitle = pca_plot_context(fit), color = cf, shape = "Role",
        x = pca_axis_label(fit, pcx, "score"), y = if (nzchar(pcy)) pca_axis_label(fit, pcy, "score") else "",
        caption = "Study fit; eligible QCs projected. Separate panels have different PCA bases and units.\nClick a point to highlight the same sample in both panels.") + pca_scatter_theme() + pca_scatter_scales(nzchar(pcy))
    return(if (nzchar(pcy)) p + ggplot2::coord_equal() else p)
  }
  if (type %in% c("density", "rle", "distribution", "feature")) {
    spec <- tryCatch(metadata_color_spec(input$metadata, color, color_mode), error = identity)
    if (inherits(spec, "error")) return(empty_plot(conditionMessage(spec)))
    color_mode <- spec$mode
    d$.color <- if (nzchar(color)) spec$values[match(d$Acquired_Sample_ID, input$metadata[[1]])] else factor(rep("Samples", nrow(d)))
  }
  if (type == "density") {
    # One bandwidth from the pre-normalization study data, independent of candidate.
    bw <- apply(log2(input$x[input$study, , drop = FALSE]), 1, function(v) if (length(unique(v)) > 1) stats::bw.nrd0(v) else NA_real_)
    bw <- stats::median(bw[is.finite(bw) & bw > 0]); if (!is.finite(bw)) bw <- .25
    limits <- range(unlist(lapply(comparison$candidates, function(z) log2(z$x)))) + c(-3, 3) * bw
    curves <- lapply(split(d, d$Acquired_Sample_ID), function(z) {
      v <- z$Log2[is.finite(z$Log2)]; if (length(v) < 2) return(NULL)
      v <- stats::density(v, bw = bw, from = limits[1], to = limits[2], n = 512)
      data.frame(x = v$x, Density = v$y, Sample = z$Acquired_Sample_ID[1], Group = z$.color[1], Highlight = z$.highlight[1])
    })
    curves <- do.call(rbind, curves)
    if (is.null(curves)) return(empty_plot("Density estimation needs at least two observed features per sample."))
    mean_curve <- stats::aggregate(Density ~ x, curves, mean)
    return(ggplot2::ggplot(curves, ggplot2::aes(x, Density, group = Sample, color = Group)) +
      ggplot2::geom_line(alpha = .25) +
      ggplot2::geom_line(data = mean_curve, ggplot2::aes(x, Density), inherit.aes = FALSE, color = "black", linewidth = 1) +
      ggplot2::geom_line(data = curves[curves$Highlight, , drop = FALSE], color = "#d95f02", linewidth = 1.2) +
      ggplot2::labs(title = "Sample density curves", subtitle = sub, x = "log2 intensity", color = color,
        caption = "Shared bandwidth/grid; black = equally weighted mean sample density. Overlap is not validation.\nImputation can create low-intensity peaks; toggle observed-only values to inspect this.") + comparison_theme() +
      comparison_color_scale(color, color_mode))
  }
  if (type %in% c("rle", "distribution")) {
    if (type == "rle") {
      # Stage-specific feature median of studies only, applied to both studies and QCs.
      x <- if (observed) evidence$normalized_observed else comparison$candidates[[method]]$x
      ref <- apply(log2(x[input$study, , drop = FALSE]), 2, stats::median, na.rm = TRUE)
      d$Log2 <- d$Log2 - ref[d$Lipid]
    }
    d$Acquired_Sample_ID <- factor(d$Acquired_Sample_ID, levels = rownames(input$x))
    shared_limits <- range(unlist(lapply(names(comparison$candidates), function(m) {
      z <- if (observed) comparison$evidence[[m]]$normalized_observed else comparison$candidates[[m]]$x
      z <- log2(z)
      if (type == "rle") z <- sweep(z, 2, apply(z[input$study, , drop = FALSE], 2, stats::median, na.rm = TRUE), "-")
      z[unique(as.character(d$Acquired_Sample_ID)), , drop = FALSE]
    })), finite = TRUE)
    return(ggplot2::ggplot(d, ggplot2::aes(Acquired_Sample_ID, Log2, fill = .color)) +
      ggplot2::coord_cartesian(ylim = shared_limits) +
      ggplot2::geom_boxplot(outlier.size = .4, na.rm = TRUE) +
      ggplot2::labs(title = if (type == "rle") "Relative log expression" else "Sample distributions", subtitle = sub,
        x = "Sample", y = if (type == "rle") "Relative log2 intensity" else "log2 intensity", fill = color,
        caption = "Offsets may be technical or biological. Do not optimize distribution alignment alone.") +
      comparison_theme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7)) +
      comparison_color_scale(color, color_mode, "fill"))
  }
  if (type == "qc_rsd") {
    z <- evidence$qc; z <- z[is.finite(z$RSD_percent), , drop = FALSE]
    if (!nrow(z)) return(empty_plot("QC RSD unavailable: too few observed measurements meet the QC detection criteria."))
    p <- ggplot2::ggplot(z, ggplot2::aes(RSD_percent))
    if (nzchar(comparison$config$pool) || nzchar(comparison$config$batch))
      p <- p + ggplot2::aes(color = interaction(Pool, Batch)) + ggplot2::labs(color = "Pool / batch")
    return(p +
      ggplot2::stat_ecdf(geom = "step") + ggplot2::geom_vline(xintercept = comparison$config$rsd_limit, linetype = 2) +
      ggplot2::labs(title = "QC reproducibility", subtitle = label, x = "Observed QC RSD (%)", y = "Fraction of eligible features",
        caption = "Before log/centering/scaling. Feature eligibility is identical across methods.\nThe RSD threshold is descriptive, not a universal quality guarantee.") + comparison_theme())
  }
  if (type == "feature") {
    if (!feature %in% colnames(input$x)) return(empty_plot(analysis_text("Select a lipid feature.", comparison)))
    z <- d[d$Lipid == feature, , drop = FALSE]
    order <- suppressWarnings(as.numeric(metadata_field(input, comparison$config$order)))
    z$Order <- order[match(z$Acquired_Sample_ID, rownames(input$x))]
    if (!nzchar(comparison$config$order)) z$Order <- match(z$Acquired_Sample_ID, rownames(input$x))
    return(ggplot2::ggplot(z, ggplot2::aes(Order, Log2, color = .color, shape = .Role)) +
      ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
      ggplot2::labs(title = feature, subtitle = sub, color = color, shape = "Role", y = "log2 intensity",
        x = if (nzchar(comparison$config$order)) comparison$config$order else "Input sample index (not injection order)") + comparison_theme() +
      comparison_color_scale(color, color_mode))
  }
  if (type == "truth") {
    truth <- input$source$truth
    if (is.null(truth)) return(empty_plot("Underlying truth is available only in the synthetic examples."))
    z <- comparison$candidates[[method]]$x[input$study, , drop = FALSE]
    t <- truth[input$study, colnames(z), drop = FALSE]
    shift <- stats::median(log2(z) - log2(t))
    zz <- matrix_long(z); zz$Truth <- as.vector(log2(t)); zz$Recovered <- as.vector(log2(z)) - shift
    return(ggplot2::ggplot(zz, ggplot2::aes(Truth, Recovered)) + ggplot2::geom_point(alpha = .12, size = .8) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "#d95f02") +
      ggplot2::labs(title = "Known synthetic truth", subtitle = label, x = "True log2 abundance", y = "Normalized log2 abundance",
        caption = "Only one global unit offset removed. Per-sample shifts are not removed.\nDeparture from the diagonal exposes technical error or erased biology.") + comparison_theme())
  }
  empty_plot("Choose a diagnostic view.")
}

scaling_scorecard <- function(comparison, transform = "log2") {
  records <- list(); k <- 0L
  for (m in names(comparison$candidates)) for (s in names(scaling_labels)) {
    fit <- tryCatch(fit_lipid_pca(candidate_result(comparison, m, transform, s), "tsn", transform, s), error = identity)
    k <- k + 1L
    if (inherits(fit, "error")) {
      records[[k]] <- data.frame(Method = m, Scaling = s, Features = NA_integer_, PC1_percent = NA_real_,
        Max_PC1_squared_loading = NA_real_, Top10_mean_missing_fraction = NA_real_, Top10_median_QC_RSD = NA_real_, Status = conditionMessage(fit))
    } else {
      load <- fit$model$rotation[, 1]^2
      top <- names(sort(load, decreasing = TRUE))[seq_len(min(10L, length(load)))]
      miss <- colMeans(!comparison$input$detected[comparison$input$study, , drop = FALSE])
      cv <- comparison$evidence[[m]]$qc
      cv <- cv$RSD_percent[cv$Lipid %in% top & is.finite(cv$RSD_percent)]
      records[[k]] <- data.frame(Method = m, Scaling = s, Features = nrow(fit$model$rotation),
        PC1_percent = fit$variance$Variance_percent[1], Max_PC1_squared_loading = max(load),
        Top10_mean_missing_fraction = mean(miss[top]), Top10_median_QC_RSD = if (length(cv)) stats::median(cv) else NA_real_, Status = "Descriptive sensitivity; not a quality rank")
    }
  }
  do.call(rbind, records)
}

processing_diagnostics <- function(result) {
  keep <- intersect(c("intensity_threshold_sensitivity", "per_sample_detection_counts", "per_lipid_missingness"), names(result$plots))
  result$plots <- result$plots[keep]
  result$plot_sizes <- result$plot_sizes[keep]
  result
}

selected_diagnostics <- function(result) {
  comp <- result$comparison
  method <- result$selection$method
  plots <- lapply(c("density", "rle", "distribution", "qc_rsd"), function(type)
    comparison_plot(comp, method, type))
  names(plots) <- c("density", "rle", "distribution", "qc_rsd")
  fit <- tryCatch(fit_lipid_pca(result, "tsn", result$selection$transform, result$selection$scaling), error = identity)
  result$selected_pca <- fit
  if (!inherits(fit, "error")) {
    pcs <- colnames(fit$model$rotation)
    plots$pca_scores <- pca_scores_plot(fit, pcs[1], if (length(pcs) > 1) pcs[2] else "", color = ".Role")
    plots$pca_loadings <- pca_loadings_plot(fit, pcs[1], if (length(pcs) > 1) pcs[2] else "")
    plots$pca_scree <- pca_scree_plot(fit)
  }
  result <- processing_diagnostics(result)
  result$plots <- c(result$plots, plots)
  result$plot_sizes <- c(result$plot_sizes, lapply(plots, function(p) list(width = 12, height = 8)))
  result
}
