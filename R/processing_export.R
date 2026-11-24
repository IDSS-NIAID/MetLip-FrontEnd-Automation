# All browser downloads use the run ID without changing internal archive paths.
metlip_download_name <- function(filename, metlip_id = NULL) {
  if (is.null(metlip_id) || !length(metlip_id) || is.na(metlip_id[1L])) return(filename)
  id <- trimws(as.character(metlip_id[1L]))
  if (!nzchar(id)) return(filename)
  id <- gsub("[^A-Za-z0-9._-]+", "_", id)
  id <- gsub("^[._]+|[._]+$", "", id)
  if (!nzchar(id)) id <- "MetLip"
  # Bound the prefix so browser downloads remain usable on common filesystems.
  id <- substr(id, 1L, 100L)
  paste0(id, "_", filename)
}

# Exports use committed preprocessing and the final PCA tab snapshot; comparison previews remain separate.
settings_table <- function(settings) {
  if (is.null(settings)) return(data.frame(Setting = character(), Value = character()))
  data.frame(Setting = names(settings), Value = vapply(settings, function(x) {
    if (is.null(x)) "Not set" else paste(x, collapse = "; ")
  }, character(1)), stringsAsFactors = FALSE)
}

# Older saved results predate occurrence-level duplicate review.
result_duplicate_audit <- function(result) {
  if (is.null(result$duplicate_audit)) empty_duplicate_audit() else result$duplicate_audit
}

# Optional fields remain readable when an older result predates notes filtering.
result_presence_filter <- function(result) {
  if (!is.null(result$presence_filter)) return(result$presence_filter)
  list(status = "not_recorded", notes_name = NULL, notes_md5 = NULL,
    approved_identifiers = character(), unmatched_approved_identifiers = character(), counts = list())
}

result_presence_filter_audit <- function(result) {
  if (is.null(result$presence_filter_audit)) empty_presence_filter_audit() else result$presence_filter_audit
}

presence_filter_status_text <- function(result) {
  presence <- result_presence_filter(result)
  labels <- c(processing_skipped = "Skipped (processing off)", applied = "Applied", skipped = "Skipped (no notes uploaded)",
    not_applicable = "Not applicable (lipidomics)", not_recorded = "Not recorded (older result)")
  status <- unname(labels[presence$status])
  if (length(status) != 1L || is.na(status)) "Not recorded (older result)" else status
}

presence_filter_summary <- function(result) {
  presence <- result_presence_filter(result)
  summary <- data.frame(Metric = "Peak-picking notes filtering", Value = presence_filter_status_text(result), stringsAsFactors = FALSE)
  if (identical(presence$status, "applied")) {
    count_labels <- c(input_features = "Signals assessed (excluding standards)",
      approved_features = "Signals approved by notes", excluded_not_approved = "Signals excluded by notes",
      collapsed_duplicates = "Additional approved signals collapsed", retained_features = "Signals retained at notes-filter stage")
    count_names <- intersect(names(count_labels), names(presence$counts))
    summary <- rbind(summary, data.frame(Metric = unname(count_labels[count_names]),
      Value = vapply(presence$counts[count_names], as.character, character(1)), stringsAsFactors = FALSE),
      data.frame(Metric = "Approved identifiers absent from analysis features",
        Value = as.character(length(presence$unmatched_approved_identifiers)), stringsAsFactors = FALSE))
  }
  rownames(summary) <- NULL
  summary
}

# Direct pipeline callers receive the same notes provenance as Shiny exports.
result_input_provenance <- function(result) {
  provenance <- result$provenance
  if (is.null(provenance)) provenance <- data.frame(Field = character(), Value = character(), stringsAsFactors = FALSE)
  presence <- result_presence_filter(result)
  if (identical(presence$status, "applied")) {
    fields <- c("Peak-picking notes filename", "Peak-picking notes MD5")
    values <- c(if (is.null(presence$notes_name)) "" else presence$notes_name,
      if (is.null(presence$notes_md5)) "" else presence$notes_md5)
    provenance <- rbind(provenance[!provenance$Field %in% fields, , drop = FALSE],
      data.frame(Field = fields, Value = values, stringsAsFactors = FALSE))
    rownames(provenance) <- NULL
  }
  provenance
}

duplicate_audit_summary <- function(audit) {
  data.frame(Metric = c("Duplicate IDs reviewed", "Duplicate IDs excluded entirely",
    "Peak-area occurrences excluded", "Metadata rows excluded"),
    Value = c(length(unique(audit$Acquired_Sample_ID)),
      length(unique(audit$Acquired_Sample_ID[audit$Decision == "Exclude ID"])),
      sum(audit$Source == "Peak-area data" & audit$Status == "Excluded"),
      sum(audit$Source == "Metadata" & audit$Status == "Excluded")),
    stringsAsFactors = FALSE)
}

# Keep the same folder names in the table-only and full-results downloads.
result_table_directories <- function() c(
  "Prepared data" = "prepared_data", "Processed data" = "processed_data", "Run details" = "run_details",
  "Quality control" = "quality_control", "Processing audit" = "processing_audit",
  "Comparison" = "comparison/summary", "PCA" = "PCA/tables")

export_file <- function(directory, ...) {
  path <- file.path(directory, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

# Mirror explicit relative paths so nested folders survive every ZIP download.
write_zip_bundle <- function(file, directory) {
  zip::zipr(zipfile = file, files = list.files(directory, recursive = TRUE),
    root = directory, mode = "mirror")
  invisible(file)
}

# One catalog supplies the results browser and table downloads. It reads only
# the committed snapshot; selecting a table never refits PCA or normalization.
result_table_catalog <- function(result) {
  if (is.null(result$selection)) stop("Select a workflow before export.")
  terms <- analysis_terms(result)
  tables <- list()
  add <- function(id, label, group, filename, description, data) {
    if (is.null(data)) return(invisible(NULL))
    if (!is.data.frame(data)) stop("Result table must be a data.frame: ", id)
    tables[[id]] <<- list(label = analysis_text(label, terms$id), group = group,
      filename = analysis_text(filename, terms$id),
      description = analysis_text(description, terms$id), data = data)
  }
  matrix_table <- function(x) data.frame(Acquired_Sample_ID = as.character(rownames(x)),
    x, check.names = FALSE, row.names = NULL)
  method <- result$selection$method
  add("data_type", "Data type", "Run details", "analysis_type.csv",
    "Selected data type and assay code for this run.",
    data.frame(Data_type = terms$label, Code = terms$code, Feature = terms$feature))
  add("supplied_wide", "Supplied measurements · wide", "Prepared data", "supplied_measurements_wide.csv",
    "Supplied numeric measurements after reviewed sample exclusions, with feature identifiers and values retained. Includes controls; no measurement processing.", result$supplied_wide)
  add("qc_eligibility", "Prepared QC eligibility", "Run details", "QC_eligibility.csv",
    "Complete prepared QCs are eligible; incomplete QCs are listed with their exclusion reason. No imputation is performed.", result$qc_eligibility)
  add("normalized_wide", "Normalized intensities · wide", "Processed data",
    paste0("processed_", method, "_wide.csv"),
    "One row per lipid and one column per study sample. Normalized intensities before log transformation, centering or scaling.",
    result$normalized_wide)
  add("normalized_long", "Normalized intensities · long", "Processed data",
    paste0("processed_", method, "_long.csv"),
    "One row per study sample and lipid, with the normalized intensity before log transformation, centering or scaling.",
    result$normalized_long)
  add("factors", "Normalization factors", "Processed data", "selected_normalization_factors.csv",
    "Multipliers and starting totals for study samples and QCs under the committed normalization.", result$normalization_factors)
  add("summary", "Processing summary", "Run details", "processing_summary.csv",
    "Sample and feature counts across processing stages.", result$summary)
  add("workflow", "Selected workflow", "Run details", "selected_workflow.csv",
    "Committed normalization, transformation, scaling, selection time and rationale.", settings_table(result$selection))
  add("processing_settings", "Processing settings", "Run details", "upstream_processing_settings.csv",
    "Import, filtering and imputation settings used for this run.", settings_table(active_processing_settings(result)))
  add("sample_roles", "Sample roles", "Run details", "sample_roles.csv",
    "Study, QC and blank classifications for the input samples.", result$sample_roles)
  add("metadata", "Sample metadata", "Run details", "sample_metadata.csv",
    "Metadata aligned to retained study samples and QCs, including their roles and imputed fractions.", result$metadata)
  add("provenance", "Input provenance", "Run details", "input_provenance.csv",
    "Input file identifiers and recorded provenance for this run.", result_input_provenance(result))
  add("presence_filter", "Peak-picking notes filtering", "Run details", "presence_filter_summary.csv",
    "Whether peak-picking notes filtering was applied, with signal counts before downstream intensity and detection filtering.",
    presence_filter_summary(result))
  add("excluded_samples", "Excluded study samples", "Run details", "excluded_study_samples.csv",
    "Unmatched study sample IDs explicitly excluded during input review. Duplicate decisions are recorded separately in Duplicate sample review.",
    data.frame(Acquired_Sample_ID = as.character(result$excluded_samples)))
  add("duplicate_audit", "Duplicate sample review", "Run details", "duplicate_sample_review.csv",
    "Applied duplicate decisions and original total input signal for each measurement occurrence. Totals include all input features and standards, omit missing values and treat negatives as zero, before filtering or normalization. Imported data rows exclude the header.",
    result_duplicate_audit(result))
  add("missingness", "Missingness by lipid", "Quality control", "per_lipid_missingness.csv",
    "Per-lipid missingness and detection evidence from processing.", result$missingness)
  add("detection_counts", "Detection counts by sample", "Quality control", "per_sample_detection_counts.csv",
    "Numbers of detected lipids per sample at the selected threshold.", result$detection_counts)
  add("normalization_scorecard", "Normalization comparison", "Comparison", "normalization_scorecard.csv",
    "The same normalization scorecard shown in the app; unavailable QC metrics remain missing.",
    comparison_scorecard_view(result$comparison))
  add("scaling_scorecard", "Scaling sensitivity", "Comparison", "scaling_sensitivity.csv",
    "Sensitivity across normalization and scaling combinations under the committed transformation.", result$scaling_scorecard)
  add("evaluation_settings", "Comparison settings", "Comparison", "evaluation_settings.csv",
    "Reference and QC eligibility settings used for comparison.",
    if (is.null(result$comparison)) NULL else settings_table(result$comparison$config[c("reference", "rsd_limit", "detection", "min_qc")]))
  evidence <- result$comparison$evidence
  qc <- lapply(names(evidence), function(name) {
    table <- evidence[[name]]$qc
    if (is.null(table)) return(NULL)
    data.frame(Method = rep(name, nrow(table)), table, check.names = FALSE, row.names = NULL)
  })
  qc <- Filter(Negate(is.null), qc)
  if (length(qc)) add("qc_precision", "QC precision by lipid", "Quality control", "QC_precision_all_methods.csv",
    "Observed-value QC precision for every normalization method; no imputed values count as repeated QC observations.",
    do.call(rbind, qc))
  add("cleaned", "Cleaned peak areas", "Processing audit", "cleaned_peak_areas.csv",
    "Cleaned peak-area input before study filtering, imputation and final normalization.", result$cleaned)
  add("standards", "Preserved internal standards", "Processing audit", "preserved_internal_standards.csv",
    "Preserved standards excluded from normalization factor estimation and PCA.", result$standards)
  add("presence_filter_audit", "Peak-picking notes audit", "Processing audit", "presence_filter_audit.csv",
    "One row per nonstandard input signal. Retained means selected at the notes-filter stage, before downstream filtering. Signal totals use all remaining study, QC and blank samples after negative values become zero; missing values are omitted. Totals are not calculated for unapproved signals. Source feature indices follow the input feature order; source positions identify the original imported row (excluding the header) or column.",
    if (identical(result$settings$processing_mode, "skip")) NULL else result_presence_filter_audit(result))
  add("removed_na", "Removed all-missing features", "Processing audit", "removed_NA_metab_list.csv",
    "Input features removed because every sample measurement was missing.", result$removed_na)
  add("lipid_name_key", "Lipid name key", "Processing audit", "lipid_name_key.csv",
    "Mapping between original and cleaned lipid names.", result$lipid_name_key)
  add("observations", "Cleaned observations · long", "Processing audit", "cleaned_observations_long.csv",
    "Observed measurements before filtering and imputation, including original missing values.", result$observations)
  add("imputed", "Filtered and imputed study data", "Processing audit", "filtered_imputed_study_long.csv",
    "Retained study measurements after the shared imputation step, before final normalization.", result$imputed)
  pca <- result$selected_pca
  if (is.list(pca) && !inherits(pca, "error") && !is.null(pca$analysis_matrix)) {
    add("analysis_matrix", "Exact PCA analysis matrix", "PCA", "selected_analysis_matrix.csv",
      "Fitted samples in rows and fitted lipids in columns. These are the exact fitted values after only the selected adjustments. No centering/scaling retains all features. See PCA settings for adjustment and QC choices.",
      matrix_table(pca$analysis_matrix))
    add("pca_scores", "PCA scores", "PCA", "PCA_scores.csv",
      "Scores for the selected PCA scope, with sample metadata and roles identifying study, fitted QC or projected QC samples.", pca_scores_export(pca))
    add("pca_loadings", "PCA loadings", "PCA", "PCA_loadings.csv",
      "Committed PCA rotation coefficients for the fitted lipid features.", pathway_export_features(pca$loadings, pca$pathway_annotation))
    add("pca_variance", paste("PCA", tolower(pca_variance_label(pca))), "PCA", "PCA_explained_variance.csv",
      paste("Eigenvalues, individual and cumulative component percentages. Basis:", pca_variance_label(pca), ". Legacy variance column names are retained."), pca$variance)
    add("pca_preprocessing", "PCA feature centers and scaling", "PCA", "selected_feature_centers_and_scaling.csv",
      "Feature centers, standard deviations and scaling divisors estimated from the samples included in the PCA fit.", pca$preprocessing)
    add("pca_removed_features", "PCA omitted features", "PCA", "PCA_constant_features_omitted.csv",
      "Constant or numerically constant features omitted from PCA; normalized intensity exports retain these features.", pca$removed_features)
    add("pca_settings", "PCA settings", "PCA", "PCA_settings.csv",
      "Settings and dimensions of the committed PCA fit.", settings_table(pca$settings))
    if (length(result$pca_view)) add("pca_plot_settings", "PCA display settings", "PCA", "selected_PCA_display_settings.csv",
      "Selected axes, metadata color and shape mappings, and other PCA display options.", settings_table(result$pca_view))
    if (!is.null(pca$qc_analysis_matrix)) add("qc_analysis_matrix", "Projected QC analysis matrix", "PCA", "QC_projected_analysis_matrix.csv",
      "Supplementary QCs transformed and scaled using the study-fitted parameters. Empty when QCs are excluded or included in the fit.",
      matrix_table(pca$qc_analysis_matrix))
  }
  # Only these tables contain an internal feature-identifier column. Metadata
  # and analysis matrices may have user columns literally named Lipid.
  if (identical(result$settings$processing_mode, "skip")) {
    tables$observations$label <- "Supplied measurements · long"
    tables$observations$filename <- "supplied_measurements_long.csv"
    tables$observations$group <- "Prepared data"
    tables$observations$description <- "Supplied measurements including controls and missing values; no measurement processing."
    for (id in intersect(c("normalized_wide", "normalized_long"), names(tables))) {
      tables[[id]]$label <- if (id == "normalized_wide") "Selected study measurements · wide" else "Selected study measurements · long"
      tables[[id]]$description <- "Study measurements after the selected normalization only. None preserves supplied values, including missing values. Transformation and scaling are recorded separately in the PCA matrix."
    }
  }
  annotation <- result$pathway_annotation
  if (is.null(annotation) && is.list(pca) && !inherits(pca, "error")) annotation <- pca$pathway_annotation
  annotations <- pathway_tables(annotation, result)
  labels <- c(pathway_input = "Uploaded pathway table", pathway_feature_annotations = "Feature pathway matches",
    pathway_unused_rows = "Unused pathway rows", pathway_summary = "Pathway match summary", pathway_settings = "Pathway provenance")
  for (id in names(annotations)) add(id, labels[[id]], "PCA", paste0(id, ".csv"),
    "Optional pathway annotations matched after processing or prepared-data loading; display only.", annotations[[id]])
  feature_tables <- c("supplied_wide", "normalized_wide", "normalized_long", "missingness", "qc_precision",
    "cleaned", "standards", "removed_na", "observations", "imputed", "pca_loadings",
    "pca_preprocessing", "pca_removed_features", "lipid_name_key", "presence_filter_audit")
  for (id in intersect(feature_tables, names(tables))) {
    columns <- if (id %in% c("lipid_name_key", "presence_filter_audit")) c("Lipid", "Lipid_original", "Lipid_base") else "Lipid"
    tables[[id]]$data <- export_feature_table(tables[[id]]$data, terms$id, columns = columns)
  }
  names(tables)[names(tables) == "lipid_name_key"] <- paste0(terms$feature, "_name_key")
  tables
}

result_table_index <- function(tables, format = c("csv", "tsv")) {
  format <- match.arg(format)
  do.call(rbind, lapply(names(tables), function(id) {
    table <- tables[[id]]
    data.frame(Table = id, Label = table$label, Group = table$group,
      Filename = file.path(unname(result_table_directories()[table$group]),
        sub("\\.csv$", paste0(".", format), table$filename)),
      Rows = nrow(table$data), Columns = ncol(table$data), Description = table$description,
      stringsAsFactors = FALSE, row.names = NULL)
  }))
}

write_result_table <- function(table, file, format = c("csv", "tsv")) {
  format <- match.arg(format)
  if (!is.list(table) || !is.data.frame(table$data)) stop("Select an available result table.")
  utils::write.table(table$data, file, sep = if (format == "csv") "," else "\t",
    row.names = FALSE, col.names = TRUE, na = "", quote = TRUE,
    qmethod = "double", fileEncoding = "UTF-8")
  invisible(file)
}

write_result_tables_bundle <- function(result, directory, format = c("csv", "tsv")) {
  format <- match.arg(format)
  terms <- analysis_terms(result)
  tables <- result_table_catalog(result)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Cannot create the table export directory.")
  index <- result_table_index(tables, format)
  for (id in names(tables)) {
    filename <- index$Filename[match(id, index$Table)]
    write_result_table(tables[[id]], export_file(directory, filename), format)
  }
  write_result_table(list(data = index), file.path(directory, paste0("table_index.", format)), format)
  pca <- result$selected_pca
  pca_status <- if (inherits(pca, "error")) paste("PCA tables unavailable:", conditionMessage(pca)) else
    if (is.null(pca) || is.null(pca$analysis_matrix)) "PCA tables unavailable: no committed PCA fit was captured." else
      "PCA tables use the captured final PCA scope; see PCA_settings.csv for QC handling and sample counts."
  writeLines(c(paste(terms$label, "result tables"), "",
    paste("Completed (UTC):", result$completed_at),
    paste("Normalization:", result$selection$method),
    paste("Transformation:", result$selection$transform),
    paste("Scaling:", result$selection$scaling), "",
    paste0("See table_index.", format, " for relative file paths, table descriptions and dimensions."),
    "Folders:",
    paste0("  ", unname(result_table_directories()), "/ - ", names(result_table_directories())), "",
    "Files contain complete tables from the committed workflow, regardless of preview filters or pagination.",
    "Text is UTF-8; missing values are empty fields. No row-number column is added.",
    "Normalized intensity tables are not log-transformed, centered or scaled.",
    "The exact PCA analysis matrix includes only the selected transformation, centering and scaling. Consult PCA settings before applying further adjustments.",
    "Blank input samples are excluded from normalization, QC precision and PCA.",
    pca_status, "",
    "The complete results archive additionally contains plots, model snapshots, comparison audit files and report source."),
    file.path(directory, "TABLES_README.txt"))
  invisible(directory)
}

write_comparison_bundle <- function(comparison, directory, transform = "log2") {
  for (method in names(comparison$candidates)) validate_normalization_method(method)
  terms <- analysis_terms(comparison)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  write <- function(x, name) utils::write.csv(x, export_file(directory, name), row.names = FALSE, na = "")
  write_feature <- function(x, name) write(export_feature_table(x, terms$id, columns = "Lipid"), name)
  write(data.frame(Data_type = terms$label, Code = terms$code, Feature = terms$feature), "summary/analysis_type.csv")
  write(comparison$scorecard, "summary/normalization_scorecard.csv")
  write(settings_table(comparison$config), "summary/evaluation_settings.csv")
  write(comparison$input$metadata, "inputs/sample_metadata.csv")
  write_feature(matrix_long(comparison$input$observed, "Observed_value"), "inputs/observed_values_before_normalization.csv")
  write_feature(matrix_long(comparison$input$x, "Imputed_value"), "inputs/shared_imputed_matrix_long.csv")
  write_feature(matrix_long(comparison$input$detected * 1, "Detected"), "inputs/detection_mask.csv")
  write_feature(data.frame(Lipid = names(comparison$input$fill), Imputation_value = comparison$input$fill), "inputs/imputation_values.csv")
  for (method in names(comparison$candidates)) {
    c <- comparison$candidates[[method]]
    method_dir <- file.path("methods", method)
    write(c$factors, file.path(method_dir, paste0(method, "_factors.csv")))
    write_feature(comparison$evidence[[method]]$qc, file.path(method_dir, paste0(method, "_QC_RSD.csv")))
    write_feature(matrix_long(c$x), file.path(method_dir, paste0(method, "_all_samples_normalized_long.csv")))
    if (!is.null(c$reference)) {
      write_feature(data.frame(Lipid = names(c$reference), Reference_value = c$reference), file.path(method_dir, "PQN_reference_profile.csv"))
      write(data.frame(Acquired_Sample_ID = c$reference_ids), file.path(method_dir, "PQN_reference_samples.csv"))
    }
    write(data.frame(Definition = c$definition), file.path(method_dir, paste0(method, "_definition.csv")))
  }
  # Plain numerical snapshot avoids live upload paths and Shiny environments.
  snapshot <- list(data_type = terms$id, assay_code = terms$code,
    candidates = comparison$candidates, config = comparison$config,
    observed = comparison$input$observed, detected = comparison$input$detected,
    metadata = comparison$input$metadata, study_ids = comparison$input$study,
    qc_ids = comparison$input$qc, evidence = comparison$evidence, transform_for_preview = transform)
  saveRDS(snapshot, export_file(directory, "snapshots", "comparison_snapshot.rds"), version = 3)
  writeLines(c(paste(terms$label, "normalization comparison"), "",
    "Folder guide:",
    "  summary/ - normalization scorecard, analysis type and evaluation settings; scaling sensitivity when included.",
    "  inputs/ - shared observations, imputed values, detection mask, imputation values and metadata.",
    "  methods/<method>/ - each candidate's normalized values, factors, QC RSD and definition.",
    "  snapshots/ - portable numerical comparison snapshot.",
    "  plots/ - current left/right views and view settings, included in the comparison download.", "",
    "All candidates start from identical filtered and imputed inputs. No method is automatically selected.",
    "QC RSD uses observed values only after sample factors and before transformation or scaling.",
    "Sample factors use the shared imputed matrix, including in QCs. Inspect coverage and imputation sensitivity.",
    "The app evaluates QCs retained after input review together as one assumed homogeneous pool across batches, with no additional manual exclusions in comparison.",
    "Each eligible feature has equal weight. Fixed QC criteria: 80% detection per sample and feature, at least 3 observed measurements per feature, and a 30% RSD threshold.",
    "Thresholds are application defaults, not universal acceptance limits.",
    "Raw CSV/RDS structures retain all configuration fields and scorecard columns for compatibility.",
    "Empty mappings and unavailable order/replicate metrics in these files are expected; the app does not infer mappings from metadata.",
    "Sample metadata remains available for plot coloring. Feature plots use input sample index, not acquisition order.",
    "Synthetic truth RMSE removes one global log2 offset only; real data have no known truth.",
    "Distribution overlap, PCA group separation and explained variance are not normalization-quality tests.",
    "No p-values, automated winner, drift correction, or predictive validation are performed."),
    file.path(directory, "COMPARISON_README.txt"))
  invisible(directory)
}

write_prepared_results_bundle <- function(result, directory) {
  if (is.null(result$selected_pca)) result$selected_pca <- tryCatch(
    fit_lipid_pca(result, "tsn", result$selection$transform, result$selection$scaling), error = identity)
  write_result_tables_bundle(result, directory)
  if (inherits(result$selected_pca, "error"))
    writeLines(conditionMessage(result$selected_pca), export_file(directory, "PCA", "ANALYSIS_MATRIX_UNAVAILABLE.txt"))
  unlink(file.path(directory, "diagnostic_plots", "factors.pdf"))
  for (name in setdiff(names(result$plots), "factors")) save_result_pdf(export_file(directory, "diagnostic_plots", paste0(name, ".pdf")),
    result$plots[[name]], width = result$plot_sizes[[name]]$width, height = result$plot_sizes[[name]]$height,
    pca = grepl("^pca_", name))
  writeLines(c(paste("MetLip", result$app_version, "— prepared-data workflow"),
    "Processing skipped. Supplied feature identifiers, zeros, negatives and missing measurements were retained.",
    "No filtering, feature-name cleaning, standard removal, negative-value replacement or imputation was performed.",
    "prepared_data/ contains supplied measurements after reviewed sample exclusions, including controls.",
    "processed_data/ contains study measurements after the selected normalization only, when that choice is valid.",
    "PCA/ contains the exact analysis matrix after explicitly selected adjustments, figures and fitted model when available.",
    "No centering/scaling preserves all feature columns. Other scaling choices subtract feature means and omit constants.",
    "Blanks are excluded from PCA; incomplete QCs are ineligible and recorded in run_details/QC_eligibility.csv.",
    "No normalization comparison or processing diagnostics were calculated.",
    "run_details/ records settings, sample roles, exclusions and input provenance.",
    "See table_index.csv for the complete table list.",
    result$analysis_error, "", result$messages), file.path(directory, "RUN_README.txt"))
  invisible(directory)
}

write_results_bundle <- function(result, directory) {
  if (identical(result$settings$processing_mode, "skip")) return(write_prepared_results_bundle(result, directory))
  if (is.null(result$selection) || is.null(result$comparison)) stop("Commit a comparison workflow before export.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  terms <- analysis_terms(result)
  write <- function(x, name) utils::write.csv(x, export_file(directory, analysis_text(name, terms$id)), row.names = FALSE, na = "")
  write_feature <- function(x, name, columns = "Lipid")
    write(export_feature_table(x, terms$id, columns = columns), name)
  method <- result$selection$method
  write(data.frame(Data_type = terms$label, Code = terms$code, Feature = terms$feature), "run_details/analysis_type.csv")
  write_feature(result$cleaned, "processing_audit/cleaned_peak_areas.csv")
  write_feature(result$standards, "processing_audit/preserved_internal_standards.csv")
  write_feature(result_presence_filter_audit(result), "processing_audit/presence_filter_audit.csv",
    columns = c("Lipid", "Lipid_original"))
  write_feature(result$removed_na, "processing_audit/removed_NA_metab_list.csv")
  write_feature(result$lipid_name_key, "processing_audit/lipid_name_key.csv", columns = c("Lipid", "Lipid_original", "Lipid_base"))
  write_feature(result$observations, "processing_audit/cleaned_observations_long.csv")
  write_feature(result$imputed, "processing_audit/filtered_imputed_study_long.csv")
  write_feature(result$normalized_long, paste0("processed_data/processed_", method, "_long.csv"))
  write_feature(result$normalized_wide, paste0("processed_data/processed_", method, "_wide.csv"))
  write(result$normalization_factors, "processed_data/selected_normalization_factors.csv")
  write(settings_table(result$selection), "run_details/selected_workflow.csv")
  write(settings_table(result$settings), "run_details/upstream_processing_settings.csv")
  write(result$summary, "run_details/processing_summary.csv")
  write(result$sample_roles, "run_details/sample_roles.csv")
  write(result_input_provenance(result), "run_details/input_provenance.csv")
  write(presence_filter_summary(result), "run_details/presence_filter_summary.csv")
  write_feature(result$missingness, "quality_control/per_lipid_missingness.csv")
  write(result$detection_counts, "quality_control/per_sample_detection_counts.csv")
  write(data.frame(Acquired_Sample_ID = result$excluded_samples), "run_details/excluded_study_samples.csv")
  write(result_duplicate_audit(result), "run_details/duplicate_sample_review.csv")
  write(result$metadata, "run_details/sample_metadata.csv")
  if (!is.null(result$scaling_scorecard)) write(result$scaling_scorecard, "comparison/summary/scaling_sensitivity.csv")
  pca <- result$selected_pca
  if (is.null(pca)) pca <- tryCatch(fit_lipid_pca(result, "tsn", result$selection$transform, result$selection$scaling), error = identity)
  if (inherits(pca, "error")) {
    writeLines(conditionMessage(pca), export_file(directory, "PCA", "ANALYSIS_MATRIX_UNAVAILABLE.txt"))
  } else {
    write(data.frame(Acquired_Sample_ID = rownames(pca$analysis_matrix), pca$analysis_matrix,
      check.names = FALSE, row.names = NULL), "PCA/tables/selected_analysis_matrix.csv")
    write_feature(pca$preprocessing, "PCA/tables/selected_feature_centers_and_scaling.csv")
    write_feature(pca$removed_features, "PCA/tables/PCA_constant_features_omitted.csv")
  }
  annotation <- result$pathway_annotation
  if (is.null(annotation) && !inherits(pca, "error")) annotation <- pca$pathway_annotation
  annotation_tables <- pathway_tables(annotation, result)
  for (name in names(annotation_tables)) write(annotation_tables[[name]], paste0("PCA/tables/", name, ".csv"))
  comp <- result$comparison
  write_comparison_bundle(comp, file.path(directory, "comparison"), result$selection$transform)
  plot_dir <- file.path(directory, "diagnostic_plots"); dir.create(plot_dir, showWarnings = FALSE)
  unlink(file.path(plot_dir, "factors.pdf"))
  for (name in setdiff(names(result$plots), "factors")) save_result_pdf(file.path(plot_dir, paste0(analysis_text(name, terms$id), ".pdf")),
    result$plots[[name]], width = result$plot_sizes[[name]]$width, height = result$plot_sizes[[name]]$height,
    pca = grepl("^pca_", name))
  if (!is.null(result$example_inputs)) {
    e <- result$example_inputs
    write(e$peak_areas, "synthetic_inputs/SYNTHETIC_input_peak_areas.csv"); write(e$metadata, "synthetic_inputs/SYNTHETIC_input_metadata.csv")
    write_feature(matrix_long(result$truth, "True_abundance"), "synthetic_inputs/SYNTHETIC_true_abundances.csv")
    write(data.frame(Acquired_Sample_ID = rownames(result$truth), Multiplier = e$technical_multiplier), "synthetic_inputs/SYNTHETIC_technical_multipliers.csv")
    write(data.frame(Scenario = result$example, Seed = e$seed), "synthetic_inputs/SYNTHETIC_settings.csv")
  }
  writeLines(c(paste("Committed", terms$label, "workflow"), paste("Completed (UTC):", result$completed_at),
    paste("Version:", result$app_version), "",
    "Folder guide:",
    "  processed_data/ - selected normalized intensities (wide and long) and normalization factors.",
    "  run_details/ - selected workflow, processing settings, summaries, sample metadata, duplicate review and provenance.",
    "  quality_control/ - per-feature missingness and per-sample detection counts.",
    "  processing_audit/ - cleaned inputs, preserved standards, removed features and shared imputed study data.",
    "  comparison/ - summaries, shared inputs, outputs grouped by method and the comparison snapshot.",
    "  PCA/tables/ - analysis matrices, scores, loadings, explained variance and fitted preprocessing settings.",
    "  PCA/plots/ and PCA/model/ - PCA figures and fitted model, when available.",
    "  diagnostic_plots/ - processing and selected-workflow diagnostic PDFs.",
    "  report/ - editable report, snapshot and figures; rendered HTML or a report status explanation.",
    if (!is.null(result$example_inputs)) "  synthetic_inputs/ - example inputs, truth and simulation settings.", "",
    "The processed intensity files are normalized but not log-transformed, centered or scaled.",
    "PCA/tables/selected_analysis_matrix.csv is the exact matrix used for the selected PCA fit, including fitted QCs only when chosen; any omitted features are listed.",
    "PCA/tables/QC_projected_analysis_matrix.csv contains supplementary QCs scaled with study-fitted parameters, and is empty when QCs are excluded or fitted.",
    "run_details/duplicate_sample_review.csv records retained and excluded duplicate occurrences by source location; imported data rows exclude the header.",
    "run_details/excluded_study_samples.csv records the separately approved exclusions of unmatched study IDs.",
    "run_details/presence_filter_summary.csv records whether peak-picking notes filtering was applied and its stage-specific counts.",
    "processing_audit/presence_filter_audit.csv records original signals and notes-filter decisions. Retained at this stage does not imply retention after intensity or study-detection filtering.",
    "Negative input values were set to zero; standards were retained separately and excluded from factor estimation/PCA.",
    "Original missing and below-threshold values share the configured small-value imputation rule. This is an assumption.",
    "Blanks do not enter sample normalization, PCA, or QC precision. QC filtering does not change study feature selection.",
    "Normalized peak-area units are not absolute concentrations.",
    "Changing group colors does not fit a different model. No group-specific factors or inferential tests are used.",
    "", "Selection rationale:", result$selection$rationale, "", "Processing messages:", result$messages,
    "", "R session:", result$analysis_session_info), file.path(directory, "RUN_README.txt"))
  invisible(directory)
}
