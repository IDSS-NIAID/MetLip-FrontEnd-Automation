# Prepared-data path: import validation is shared, measurement processing is bypassed.
is_prepared_result <- function(result) identical(result$settings$processing_mode, "skip")

retain_prepared_data <- function(inspected, exclude_unmatched = FALSE) {
  dat <- inspected$data
  if (anyDuplicated(dat$Lipid))
    stop("Prepared data require unique feature identifiers. Rename duplicate features in the input; none were combined or removed.")
  excluded <- if (exclude_unmatched) inspected$unmatched_study_samples else character()
  dat <- dat[, c("Lipid", setdiff(inspected$sample_ids, excluded)), drop = FALSE]
  for (j in seq.int(2L, ncol(dat))) {
    text <- trimws(dat[[j]])
    missing <- is.na(text) | text %in% c("", "NA", "N/A")
    values <- suppressWarnings(as.numeric(text))
    if (any(!missing & !is.finite(values))) stop("Prepared measurements must be numeric and finite, or missing.")
    values[missing] <- NA_real_
    dat[[j]] <- values
  }
  x <- t(as.matrix(dat[-1L])); colnames(x) <- dat$Lipid
  study_ids <- intersect(inspected$study_sample_ids, rownames(x))
  qc_ids <- intersect(inspected$qc_sample_ids, rownames(x))
  qc_complete <- rowSums(!is.finite(x[qc_ids, , drop = FALSE])) == 0L
  eligibility <- data.frame(Acquired_Sample_ID = qc_ids, Eligible = qc_complete,
    Reason = ifelse(qc_complete, "Complete supplied measurements", "Missing measurements; no imputation performed"),
    row.names = NULL)
  presence <- empty_presence_filter(inspected$settings)
  presence$status <- "processing_skipped"
  list(supplied_wide = dat, supplied_study = x[study_ids, , drop = FALSE],
    supplied_qc = x[qc_ids[qc_complete], , drop = FALSE],
    observations = matrix_long(x, "Value"), qc_eligibility = eligibility,
    plots = list(), plot_sizes = list(), settings = validate_settings(inspected$settings),
    summary = data.frame(Metric = c("Processing", "Retained features", "Retained study samples",
      "Blank controls excluded from PCA", "Complete QC samples", "Incomplete QC samples", "Imputed measurements"),
      Value = c("Skipped — supplied measurements retained", ncol(x), length(study_ids),
        length(intersect(inspected$blank_sample_ids, rownames(x))), sum(qc_complete), sum(!qc_complete), 0)),
    metadata = metadata_with_data_batch(inspected$metadata, inspected$sample_roles),
    sample_roles = inspected$sample_roles, excluded_samples = excluded,
    duplicate_audit = inspected$duplicate_audit, duplicate_resolutions = inspected$duplicate_resolutions,
    duplicate_signal = inspected$duplicate_signal, presence_filter = presence,
    presence_filter_audit = empty_presence_filter_audit(),
    data_name = inspected$data_name, metadata_name = inspected$metadata_name,
    orientation = inspected$orientation, unused_metadata = inspected$unused_metadata,
    messages = c(inspected$messages,
      "Processing skipped: no feature filtering, name cleaning, standard removal, negative-value replacement, imputation or automatic normalization.",
      if (any(!qc_complete)) paste("Incomplete QCs are ineligible for PCA:", paste(qc_ids[!qc_complete], collapse = ", "))))
}

prepared_selection <- function(result, method = "none", transform = "none", scaling = "as_is") {
  stopifnot(is_prepared_result(result))
  validate_normalization_method(method)
  if (!transform %in% c("none", "log2") ||
      !scaling %in% names(scaling_labels)) stop("Choose an available PCA adjustment.")
  r <- result
  r$analysis_error <- NULL; r$selected_pca <- NULL
  r[c("normalized_long", "normalized_wide", "normalization_factors", "qc_normalized")] <- list(NULL, NULL, NULL, NULL)
  r$selection <- list(method = method, transform = transform, scaling = scaling,
    rationale = "Prepared data: processing skipped; only explicitly selected PCA adjustments applied.",
    normalization_definition = if (method == "none") "Identity; supplied measurements retained" else unname(normalization_labels[method]),
    selected_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  x <- rbind(r$supplied_study, r$supplied_qc)
  # Missing measurements can be retained/exported under identity normalization;
  # the PCA fitter reports them rather than changing or silently dropping data.
  candidate <- tryCatch({
    if (method == "none") list(x = x, factors = data.frame(Acquired_Sample_ID = rownames(x),
      Multiplier = rep(1, nrow(x)), row.names = NULL)) else
      normalize_matrix(x, rownames(r$supplied_study), method)
  }, error = identity)
  if (inherits(candidate, "error")) {
    r$analysis_error <- conditionMessage(candidate)
    r$selected_pca <- candidate
  } else {
    if (!is.null(candidate$definition)) r$selection$normalization_definition <- candidate$definition
    study <- candidate$x[rownames(r$supplied_study), , drop = FALSE]
    r$normalized_long <- matrix_long(study)
    r$normalized_wide <- data.frame(Lipid = colnames(study), t(study), check.names = FALSE, row.names = NULL)
    r$normalization_factors <- candidate$factors
    r$qc_normalized <- candidate$x[rownames(r$supplied_qc), , drop = FALSE]
  }
  m <- as.data.frame(r$metadata, check.names = FALSE)
  m <- m[match(rownames(x), as.character(m[[1L]])), , drop = FALSE]
  m[[1L]] <- rownames(x); rownames(m) <- NULL
  m$.Role <- ifelse(rownames(x) %in% rownames(r$supplied_study), "Study", "QC")
  m$.Imputed_fraction <- 0
  r$metadata <- m
  r
}
