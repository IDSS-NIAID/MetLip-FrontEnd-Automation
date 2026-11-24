# Pure numerical functions. Rows are samples, columns are lipid features.
normalization_catalog <- c(none = "No additional normalization", tsn = "Total sum", median = "Median", pqn = "PQN")
# Shared availability for direct R calls, selectors, comparisons and exports.
enabled_normalization_methods <- c("none", "tsn", "median", "pqn")
normalization_labels <- normalization_catalog[enabled_normalization_methods]
scaling_labels <- c(as_is = "None — no centering or scaling", none = "Center only", pareto = "Pareto", unit = "Z-scaling (unit variance)")

normalization_method_error <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method))
    return("Choose an available normalization method.")
  if (!method %in% names(normalization_catalog)) return("Unknown normalization method.")
  if (!method %in% enabled_normalization_methods)
    return(paste0(normalization_catalog[[method]], " normalization is disabled in this build."))
  NULL
}

validate_normalization_method <- function(method) {
  error <- normalization_method_error(method)
  if (!is.null(error)) stop(error, call. = FALSE)
  invisible(method)
}

long_matrix <- function(dat, value, samples = unique(dat$Acquired_Sample_ID), features = unique(dat$Lipid)) {
  if (anyDuplicated(dat[c("Acquired_Sample_ID", "Lipid")])) stop("Duplicate sample/feature pairs.")
  x <- matrix(NA_real_, length(samples), length(features), dimnames = list(samples, features))
  i <- match(dat$Acquired_Sample_ID, samples); j <- match(dat$Lipid, features)
  ok <- !is.na(i) & !is.na(j)
  x[cbind(i[ok], j[ok])] <- dat[[value]][ok]
  x
}

matrix_long <- function(x, value = "Value_normalized") {
  d <- data.frame(Acquired_Sample_ID = rep(rownames(x), times = ncol(x)),
                  Lipid = rep(colnames(x), each = nrow(x)), stringsAsFactors = FALSE)
  d[[value]] <- as.vector(x)
  d
}

comparison_input <- function(result) {
  study <- long_matrix(result$imputed, "Value_imputed")
  features <- colnames(study)
  roles <- result$sample_roles
  qc_ids <- roles$Acquired_Sample_ID[roles$Role == "QC"]
  raw <- long_matrix(result$observations, "Value", c(rownames(study), qc_ids), features)
  detected <- is.finite(raw) & raw >= result$settings$intensity_threshold
  observed <- raw; observed[!detected] <- NA_real_
  fill <- apply(observed[rownames(study), , drop = FALSE], 2, min, na.rm = TRUE) * result$settings$impute_fraction
  x <- raw
  for (j in seq_len(ncol(x))) x[!detected[, j], j] <- fill[j]
  x[rownames(study), ] <- study
  if (any(!is.finite(x) | x <= 0)) stop("Comparison input must be finite and positive after the shared imputation step.")
  # All metadata are aligned by ID; absent control metadata remain missing.
  metadata <- data.frame(Acquired_Sample_ID = rownames(x), check.names = FALSE)
  if (!is.null(result$metadata)) {
    m <- as.data.frame(result$metadata, check.names = FALSE)
    if (any(c(".Role", ".Imputed_fraction") %in% names(m)))
      stop("Metadata columns .Role and .Imputed_fraction are reserved for diagnostics; rename those input columns.")
    at <- match(rownames(x), as.character(m[[1]]))
    # Preserve the uploaded ID header so a later Acquired_Sample_ID metadata
    # field cannot replace the actual sample IDs used for alignment.
    metadata <- m[at, , drop = FALSE]
    metadata[[1L]] <- rownames(x)
    rownames(metadata) <- NULL
  }
  metadata$.Role <- ifelse(rownames(x) %in% qc_ids, "QC", "Study")
  metadata$.Imputed_fraction <- rowMeans(!detected)
  list(x = x, observed = observed, detected = detected, study = rownames(study), qc = qc_ids,
       metadata = metadata, fill = fill, source = result, data_type = analysis_terms(result)$id)
}

normalize_matrix <- function(x, study_ids, method = "tsn", reference_ids = study_ids) {
  validate_normalization_method(method)
  if (!is.matrix(x) || !is.numeric(x) || !nrow(x) || !ncol(x) || any(!is.finite(x)))
    stop("Normalization requires a complete finite numeric matrix; no imputation was performed.")
  if (method != "none" && any(x <= 0))
    stop("TSN, median and PQN normalization require strictly positive values. Choose No additional normalization for signed or zero-valued prepared data.")
  if (is.null(rownames(x)) || anyDuplicated(rownames(x)) || !length(study_ids) || !all(study_ids %in% rownames(x)))
    stop("Valid, unique sample IDs and study IDs are required.")
  totals <- rowSums(x)
  if (method != "none" && any(!is.finite(totals))) stop("Sample totals overflow; check input units.")
  target <- mean(totals[study_ids]); med <- apply(x, 1, stats::median)
  multiplier <- rep(1, nrow(x)); names(multiplier) <- rownames(x)
  reference <- NULL; quotient <- rep(NA_real_, nrow(x))
  if (method == "tsn") multiplier <- target / totals
  if (method == "median") multiplier <- stats::median(med[study_ids]) / med
  if (method == "pqn") {
    if (!length(reference_ids) || !all(reference_ids %in% rownames(x))) stop("PQN needs a nonempty reference sample set.")
    preliminary <- sweep(x, 1, target / totals, "*")
    reference <- apply(preliminary[reference_ids, , drop = FALSE], 2, stats::median)
    quotient <- apply(sweep(preliminary, 2, reference, "/"), 1, stats::median)
    multiplier <- target / totals / quotient
  }
  if (any(!is.finite(multiplier) | multiplier <= 0)) stop("Invalid normalization multiplier.")
  out <- sweep(x, 1, multiplier, "*")
  if (any(!is.finite(out)) || (method != "none" && any(out <= 0))) stop("Normalization produced invalid values.")
  list(method = method, x = out, multiplier = multiplier, reference = reference,
       reference_ids = if (method == "pqn") reference_ids else character(),
       factors = data.frame(Acquired_Sample_ID = rownames(x), Multiplier = unname(multiplier),
         Total_before = totals, Median_before = med, PQN_quotient = quotient, row.names = NULL),
       definition = switch(method, none = "Identity; no additional sample normalization",
         tsn = "x / sample total * mean study-sample total",
         median = "x / sample median * median study-sample median",
         pqn = "Common-total prescaling; feature median reference; median positive quotient; prescaled x / quotient"))
}

comparison_config <- function(pool = "", batch = "", order = "", group = "", replicate = "",
                              exclude_qc = character(), reference = "study", reference_pool = "",
                              rsd_limit = 30, detection = 0.8, min_qc = 3L, prior = "Not specified") {
  if (!reference %in% c("study", "qc")) stop("PQN reference must be study or qc.")
  if (!is.finite(rsd_limit) || rsd_limit <= 0 || !is.finite(detection) || detection <= 0 || detection > 1 ||
      !is.finite(min_qc) || min_qc < 3 || min_qc != as.integer(min_qc)) stop("Invalid QC criteria (at least 3 QC observations required).")
  list(pool = pool, batch = batch, order = order, group = group, replicate = replicate,
       exclude_qc = exclude_qc, reference = reference, reference_pool = reference_pool,
       rsd_limit = rsd_limit, detection = detection, min_qc = as.integer(min_qc), prior = prior)
}

metadata_field <- function(input, field, fallback = NA_character_) {
  if (is.null(field) || !nzchar(field)) return(rep(fallback, nrow(input$x)))
  if (!field %in% names(input$metadata)) stop("Metadata field is unavailable: ", field)
  x <- trimws(as.character(input$metadata[[field]])); x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

evaluate_normalization <- function(input, candidate, config) {
  validate_normalization_method(candidate$method)
  # The same raw detection mask defines every candidate's eligible QC entries.
  ids <- rownames(input$x); meta <- input$metadata
  is_qc <- ids %in% setdiff(input$qc, config$exclude_qc)
  pool <- metadata_field(input, config$pool, "Single assumed pool")
  batch <- metadata_field(input, config$batch, "All batches")
  usable <- is_qc & !is.na(pool) & !is.na(batch) & rowMeans(input$detected) >= config$detection
  strata <- interaction(match(pool, unique(pool)), match(batch, unique(batch)), drop = TRUE, lex.order = TRUE)
  obs <- sweep(input$observed, 1, candidate$multiplier, "*")
  records <- list(); k <- 0L
  for (s in unique(as.character(strata[usable]))) {
    index <- which(usable & as.character(strata) == s)
    if (length(index) < config$min_qc) next
    for (j in seq_len(ncol(obs))) {
      v <- obs[index, j]; detected <- is.finite(v)
      eligible <- sum(detected) >= config$min_qc && mean(detected) >= config$detection
      cv <- if (eligible && mean(v[detected]) > 0) 100 * stats::sd(v[detected]) / mean(v[detected]) else NA_real_
      k <- k + 1L
      records[[k]] <- data.frame(Pool = pool[index[1]], Batch = batch[index[1]], Lipid = colnames(obs)[j],
        QC_count = length(index), Observed_count = sum(detected), Detection_fraction = mean(detected),
        RSD_percent = cv, Eligible = eligible, stringsAsFactors = FALSE)
    }
  }
  table <- if (length(records)) do.call(rbind, records) else data.frame(Pool = character(), Batch = character(),
    Lipid = character(), QC_count = integer(), Observed_count = integer(), Detection_fraction = numeric(),
    RSD_percent = numeric(), Eligible = logical())
  valid <- table$RSD_percent[is.finite(table$RSD_percent)]
  # Monotonic drift is descriptive; it does not detect every nonlinear pattern.
  run <- suppressWarnings(as.numeric(metadata_field(input, config$order)))
  correlations <- numeric()
  for (s in unique(as.character(strata[usable]))) {
    index <- which(usable & as.character(strata) == s & is.finite(run))
    if (length(index) < max(4L, config$min_qc) || length(unique(run[index])) < 3) next
    for (j in seq_len(ncol(obs))) {
      keep <- is.finite(obs[index, j]); ix <- index[keep]
      if (length(ix) >= max(4L, config$min_qc) && length(unique(run[ix])) > 1 && stats::sd(obs[ix, j]) > 0)
        correlations <- c(correlations, abs(stats::cor(run[ix], obs[ix, j], method = "spearman")))
    }
  }
  correlations <- correlations[is.finite(correlations)]
  replicate <- metadata_field(input, config$replicate)
  rep_sd <- numeric()
  for (r in unique(replicate[ids %in% input$study & !is.na(replicate)])) {
    ix <- which(ids %in% input$study & !is.na(replicate) & replicate == r)
    if (length(ix) < 2L) next
    z <- log2(obs[ix, , drop = FALSE])
    rep_sd <- c(rep_sd, apply(z, 2, function(v) if (sum(is.finite(v)) >= 2) stats::sd(v, na.rm = TRUE) else NA_real_))
  }
  rep_sd <- rep_sd[is.finite(rep_sd)]
  truth_error <- NA_real_
  truth <- input$source$truth
  if (!is.null(truth)) {
    a <- log2(candidate$x[input$study, , drop = FALSE]); b <- log2(truth[input$study, colnames(a), drop = FALSE])
    delta <- a - b
    # One scalar offset for the entire matrix permits only arbitrary unit changes.
    truth_error <- sqrt(mean((delta - stats::median(delta))^2))
  }
  assessment <- if (!length(valid)) "QC evidence unavailable" else "Review QC, controls and biological assumptions"
  if (config$reference == "qc" && candidate$method == "pqn") assessment <- paste(assessment, "(QC reference: in-sample assessment)")
  summary <- data.frame(Method = candidate$method, Label = unname(normalization_labels[candidate$method]),
    QC_used = sum(usable), QC_excluded_or_unusable = sum(is_qc) - sum(usable) + sum(ids %in% config$exclude_qc),
    Eligible_feature_strata = length(valid), Median_QC_RSD_percent = if (length(valid)) stats::median(valid) else NA_real_,
    Percent_meeting_RSD_limit = if (length(valid)) 100 * mean(valid <= config$rsd_limit) else NA_real_,
    Median_abs_QC_order_rho = if (length(correlations)) stats::median(correlations) else NA_real_,
    Median_replicate_log2_SD = if (length(rep_sd)) stats::median(rep_sd) else NA_real_,
    Truth_log2_RMSE = truth_error, Assessment = assessment, stringsAsFactors = FALSE)
  list(summary = summary, qc = table, usable_qc = ids[usable], normalized_observed = obs)
}

run_comparison <- function(result, config = comparison_config()) {
  input <- comparison_input(result)
  reference_ids <- input$study
  if ("pqn" %in% enabled_normalization_methods && config$reference == "qc") {
    pool <- metadata_field(input, config$pool, "Single assumed pool")
    reference_ids <- setdiff(input$qc, config$exclude_qc)
    if (nzchar(config$pool)) reference_ids <- intersect(reference_ids, rownames(input$x)[!is.na(pool) & pool == config$reference_pool])
    reference_ids <- intersect(reference_ids, rownames(input$x)[rowMeans(input$detected) >= config$detection])
    if (length(reference_ids) < config$min_qc) stop("The selected PQN pool needs enough eligible QC samples. Use the study reference or revise the mapping.")
  }
  candidates <- lapply(names(normalization_labels), function(m) normalize_matrix(input$x, input$study, m, reference_ids))
  names(candidates) <- names(normalization_labels)
  evidence <- lapply(candidates, function(c) evaluate_normalization(input, c, config))
  list(input = input, candidates = candidates, evidence = evidence, config = config,
       scorecard = do.call(rbind, lapply(evidence, `[[`, "summary")))
}

candidate_result <- function(comparison, method, transform = "log2", scaling = "pareto", rationale = "Preview") {
  validate_normalization_method(method)
  r <- comparison$input$source; c <- comparison$candidates[[method]]
  if (is.null(c)) stop("Choose an available normalization method.")
  study <- comparison$input$study
  r$normalized_long <- matrix_long(c$x[study, , drop = FALSE])
  r$normalized_wide <- data.frame(Lipid = colnames(c$x), t(c$x[study, , drop = FALSE]), check.names = FALSE, row.names = NULL)
  r$normalization_factors <- c$factors
  r$qc_normalized <- c$x[comparison$evidence[[method]]$usable_qc, , drop = FALSE]
  r$qc_imputed <- comparison$input$x[rownames(r$qc_normalized), , drop = FALSE]
  r$metadata <- comparison$input$metadata
  r$selection <- list(method = method, transform = transform, scaling = scaling, rationale = rationale,
    normalization_definition = c$definition, selected_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  r$comparison <- comparison
  # Avoid recursive copies in exported snapshots.
  r$comparison$input$source <- NULL
  r
}
