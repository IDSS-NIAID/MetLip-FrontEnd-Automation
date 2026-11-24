# Portable presentation helpers. Only the saved snapshot is read; no analysis is run.
report_value <- function(x, fallback = "Not recorded") {
  if (is.null(x) || !length(x) || all(is.na(x)) || !nzchar(trimws(as.character(x[1L])))) return(fallback)
  paste(as.character(x), collapse = "; ")
}
report_setting <- function(table, key, fallback = "Not recorded") {
  if (is.null(table) || !all(c("Setting", "Value") %in% names(table))) return(fallback)
  report_value(table$Value[table$Setting == key], fallback)
}
report_number <- function(x, digits = 1L) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) return("Not available")
  if (x != 0 && abs(x) < 10^(-digits)) return(format(signif(x, 3), scientific = TRUE, trim = TRUE))
  format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
}
report_label <- function(x) {
  labels <- c(QC_used = "Eligible QC samples", QC_excluded_or_unusable = "Excluded or unusable QCs",
    Eligible_features = "Eligible features", Eligible_feature_strata = "Eligible feature/pool/batch combinations",
    Median_QC_RSD_percent = "Median QC RSD (%)", Percent_meeting_RSD_limit = "Meeting recorded RSD threshold (%)",
    Acquired_Sample_ID = "Sample ID", pct_missing = "Missing or below threshold (%)", presence = "Detection fraction",
    PC1_percent = "PC1 contribution (%)", Max_PC1_squared_loading = "Maximum squared PC1 loading",
    Top10_mean_missing_fraction = "Top 10: mean nondetection fraction", Top10_median_QC_RSD = "Top 10: median QC RSD (%)",
    Variance_percent = "Component contribution (%)", Cumulative_percent = "Cumulative contribution (%)",
    rsd_limit = "QC RSD threshold (%)", detection = "QC detection fraction", min_qc = "Minimum observed QCs",
    intensity_threshold = "Intensity threshold (input peak-area units)", min_presence = "Minimum study detection fraction",
    impute_fraction = "Imputation fraction of feature minimum", method = "Sample normalization",
    transform = "PCA transformation", scaling = "PCA centering / scaling", reference = "PQN reference population")
  out <- unname(labels[x]); missing <- is.na(out)
  out[missing] <- gsub("_", " ", x[missing], fixed = TRUE)
  out
}
report_emit <- function(...) {
  html <- as.character(htmltools::tagList(...))
  # Raw blocks keep Pandoc from treating deeply indented image tags as code.
  # A delimiter longer than any uploaded backtick run cannot be closed by text.
  runs <- regmatches(html, gregexpr("`+", html))[[1L]]
  fence <- paste(rep("`", max(c(3L, nchar(runs) + 1L))), collapse = "")
  cat("\n", fence, "{=html}\n", html, "\n", fence, "\n\n", sep = "")
}
report_note <- function(text) report_emit(htmltools::tags$p(class = "report-note", text))
report_table <- function(x, max_rows = NULL, max_columns = NULL, labels = TRUE, caption = NULL) {
  if (is.null(x)) return(report_note("Not recorded in this snapshot."))
  if (!nrow(x)) return(report_note("No entries in the recorded table."))
  nr <- nrow(x); nc <- ncol(x)
  if (!is.null(max_rows)) x <- head(x, max_rows)
  if (!is.null(max_columns)) x <- x[, seq_len(min(max_columns, nc)), drop = FALSE]
  numeric <- vapply(x, is.numeric, logical(1))
  headers <- if (labels) report_label(names(x)) else names(x)
  cells <- function(j) {
    z <- x[[j]]
    if (is.numeric(z)) return(vapply(z, report_number, character(1),
      digits = if (grepl("percent|pct_|RSD", names(x)[j], ignore.case = TRUE)) 1L else 3L))
    z <- as.character(z); z[is.na(z)] <- "Missing"
    z
  }
  values <- lapply(seq_along(x), cells)
  tags <- htmltools::tags
  report_emit(tags$div(class = "report-table-wrap", tabindex = "0", role = "region",
    `aria-label` = if (is.null(caption)) "Scrollable data table" else caption,
    tags$table(class = "report-table",
      if (!is.null(caption)) tags$caption(caption),
      tags$thead(tags$tr(lapply(seq_along(x), function(j) tags$th(scope = "col",
        class = if (numeric[j]) "numeric" else NULL, title = names(x)[j], headers[j])))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(
        class = if ("Selection" %in% names(x) && identical(x$Selection[i], "Selected")) "selected-method" else NULL,
        lapply(seq_along(x), function(j) tags$td(class = if (numeric[j]) "numeric" else NULL, values[[j]][i]))))))))
  if (nrow(x) < nr || ncol(x) < nc) report_emit(tags$p(class = "report-note",
    paste("Preview:", nrow(x), "of", nr, "rows and", ncol(x), "of", nc, "columns."),
    " Complete tables are supplied separately; filenames appear in the ", tags$a(href = "#data-index", "data index"), "."))
}
report_settings <- function(x, caption = NULL) {
  if (!is.null(x) && "Setting" %in% names(x)) x$Setting <- report_label(x$Setting)
  report_table(x, caption = caption)
}
report_summary_metric <- function(r, metric) {
  if (is.null(r$summary)) return(NA_real_)
  suppressWarnings(as.numeric(r$summary$Value[match(metric, r$summary$Metric)]))
}
report_selected_qc <- function(r) {
  x <- r$comparison_scorecard
  method <- report_setting(r$selection, "method", "")
  if (is.null(x) || !"Method" %in% names(x)) return(NULL)
  x[x$Method == method, , drop = FALSE]
}
report_qc_state <- function(r) {
  if (!is.null(r$qc_assessment)) return(r$qc_assessment)
  if (isTRUE(r$processing_skipped)) return(list(status = "Not run", reason = "QC precision comparison was not run in prepared-data mode."))
  x <- report_selected_qc(r)
  if (is.null(x) || !nrow(x)) return(list(status = "Not recorded", reason = "QC evidence for the selected method was not recorded in this snapshot."))
  if (any(is.finite(x$Median_QC_RSD_percent))) return(list(status = "Available", reason = "Observed QC precision is recorded for the selected normalization."))
  list(status = "Not assessable", reason = "No finite observed QC RSD is recorded. Detailed eligibility reasons were not saved in this older snapshot.")
}
report_modules <- function(r) {
  present <- function(x) !is.null(x) && nrow(x) > 0L
  qc <- report_qc_state(r)
  detection <- if (isTRUE(r$processing_skipped)) "Not run" else
    if (present(r$detection_counts) || present(r$missingness)) "Available" else "Not recorded"
  normalization <- if (!is.null(r$analysis_error)) "Not assessable" else
    if (present(r$selection) && present(r$normalization_factors)) "Available" else "Not recorded"
  pca <- if (is.null(r$pca)) "Not recorded" else if (isTRUE(r$pca$available)) "Available" else "Not assessable"
  data.frame(Module = c("Sample and feature accounting", "Detection and missingness", "Normalization", "QC precision", "PCA"),
    Anchor = c("sample-accounting", "detection-missingness", "normalization-qc", "qc-evidence", "pca-results"),
    Status = c(if (present(r$summary)) "Available" else "Not recorded", detection, normalization, qc$status, pca),
    Evidence = c(paste(report_value(r$normalized_dimensions["samples"]), "retained study samples;",
        report_value(r$normalized_dimensions["features"]), "retained features."),
      if (isTRUE(r$processing_skipped)) "Prepared-data mode: filtering and imputation were skipped." else
        paste("Imputed study measurements:", report_number(report_summary_metric(r, "Imputed measurements"), 0),
          "of", report_number(report_summary_metric(r, "Final measurements"), 0), "retained measurements."),
      if (!is.null(r$analysis_error)) r$analysis_error else paste("Selected method:", report_method(report_setting(r$selection, "method"))),
      qc$reason,
      if (isTRUE(r$pca$available)) paste(report_value(r$pca$variance_label), ";", report_setting(r$pca$settings, "sample_scope")) else
        report_value(r$pca$message, "PCA was not recorded in this snapshot.")), stringsAsFactors = FALSE)
}
report_badge <- function(status) htmltools::tags$span(class = paste("evidence-status",
  if (status == "Available") "evidence-available" else "evidence-neutral"), status)
report_module_status <- function(modules, module) {
  x <- modules[modules$Module == module, , drop = FALSE]
  report_emit(htmltools::tags$p(class = "module-status", report_badge(x$Status), " ", x$Evidence))
}
report_evidence_summary <- function(r, modules) {
  tags <- htmltools::tags
  report_emit(tags$div(class = "report-table-wrap", tabindex = "0", role = "region",
    `aria-label` = "Evidence availability summary", tags$table(class = "report-table evidence-summary",
    tags$caption("Evidence availability for the recorded workflow"),
    tags$thead(tags$tr(tags$th(scope = "col", "Module"), tags$th(scope = "col", "Availability"), tags$th(scope = "col", "Recorded evidence"))),
    tags$tbody(lapply(seq_len(nrow(modules)), function(i) tags$tr(
      tags$th(scope = "row", tags$a(href = paste0("#", modules$Anchor[i]), modules$Module[i])),
      tags$td(report_badge(modules$Status[i])), tags$td(modules$Evidence[i])))))))
  x <- report_selected_qc(r)
  if (!is.null(x) && nrow(x)) {
    cols <- intersect(c("QC_used", "Eligible_features", "Eligible_feature_strata", "Median_QC_RSD_percent", "Percent_meeting_RSD_limit"), names(x))
    report_table(x[, cols, drop = FALSE], caption = "Selected normalization: QC evidence")
  }
}
report_method <- function(x) {
  choices <- c(none = "No additional normalization", tsn = "Total sum", median = "Median", pqn = "PQN")
  if (length(x) == 1L && x %in% names(choices)) unname(choices[x]) else x
}
report_workflow <- function(r) {
  method <- report_method(report_setting(r$selection, "method"))
  transform <- report_setting(r$selection, "transform")
  scales <- c(as_is = "None — no centering or scaling", none = "Center only", pareto = "Pareto", unit = "Unit variance (Z-scaling)")
  scale <- report_setting(r$selection, "scaling")
  if (scale %in% names(scales)) scale <- unname(scales[scale])
  report_emit(htmltools::tags$div(class = "workflow-strip",
    htmltools::tags$span(htmltools::tags$strong("Sample normalization: "), method),
    htmltools::tags$span(htmltools::tags$strong("PCA transformation: "), if (transform == "none") "None" else transform),
    htmltools::tags$span(htmltools::tags$strong("PCA centering / scaling: "), scale)))
}
report_comparison <- function(r) {
  x <- r$comparison_scorecard
  if (!is.null(x)) {
    # The raw exported scorecard is unchanged; imperative assessment text is not report prose.
    x <- x[, setdiff(names(x), "Assessment"), drop = FALSE]
    x$Selection <- ifelse(x$Method == report_setting(r$selection, "method", ""), "Selected", "")
    if ("Label" %in% names(x)) { x$Method <- x$Label; x$Label <- NULL }
    x <- x[, c("Selection", setdiff(names(x), "Selection")), drop = FALSE]
  }
  report_table(x, caption = "Normalization comparison — descriptive evidence")
}
report_qc_context <- function(r) {
  config <- r$evaluation_context
  setting <- function(key) {
    if (!is.null(config) && key %in% names(config)) return(report_value(config[[key]], "None"))
    report_setting(r$evaluation_settings, key)
  }
  detection <- suppressWarnings(as.numeric(setting("detection")))
  threshold <- setting("rsd_limit")
  report_note(paste0("Recorded eligibility: detection fraction ", if (is.finite(detection)) report_number(100 * detection) else "Not recorded",
    if (is.finite(detection)) "%" else "", " per QC sample and feature; minimum observed QCs per feature within each pool/batch: ",
    setting("min_qc"), ". Descriptive RSD threshold: ", threshold, if (threshold == "Not recorded") "." else "%."))
  report_note(paste("PQN reference population:", setting("reference"), "."))
  if (is.null(config)) {
    report_note("QC pool, batch and manual-exclusion settings were not recorded in this older snapshot.")
  } else {
    report_note(paste("QC pool:", if (!nzchar(config$pool)) "single assumed homogeneous pool" else paste("metadata field", config$pool),
      "| Batch:", if (!nzchar(config$batch)) "all batches evaluated together" else paste("metadata field", config$batch),
      "| Explicit QC exclusions:", length(config$exclude_qc), "."))
    if (identical(config$reference, "qc")) report_note(paste("PQN uses a QC-derived reference; QC precision is an in-sample assessment. Reference pool:",
      report_value(config$reference_pool, "All eligible reference QCs"), "."))
  }
}
report_figure_state <- new.env(parent = emptyenv())
report_figure_state$number <- 0L
report_figures <- function(r, keys) {
  plots <- r$plots
  if (is.null(plots) || !nrow(plots)) return(invisible(NULL))
  for (key in intersect(keys, plots$key)) {
    i <- match(key, plots$key)
    title <- plots$title[i]
    # Fixed internal keys, not uploaded labels, supply the fragment identifiers.
    anchor <- if (key == "qc_rsd") "qc-precision" else gsub("_", "-", key, fixed = TRUE)
    cat("\n\n### ", htmltools::htmlEscape(title), " {#", anchor, "}\n\n", sep = "")
    report_figure_state$number <- report_figure_state$number + 1L
    number <- report_figure_state$number
    legends <- if ("legend_paths" %in% names(plots)) plots$legend_paths[[i]] else character()
    description <- if ("explanation" %in% names(plots)) report_value(plots$explanation[i], "") else ""
    if (!nzchar(description) || grepl("Committed normalization workflow", description, fixed = TRUE))
      description <- "Detailed figure context was not recorded in this older snapshot. The original axes and caption are retained."
    tags <- htmltools::tags
    report_emit(tags$figure(class = "report-figure", `aria-labelledby` = paste0("figure-caption-", number),
      tags$div(class = "metlip-figure", tags$div(class = "metlip-figure-row",
        tags$div(class = "metlip-figure-main", tags$img(src = plots$path[i], alt = paste(title, description))),
        if (length(legends)) tags$div(class = "metlip-legend-slot", tags$div(class = "metlip-legend-panel",
          tabindex = "0", role = "region", `aria-label` = paste(title, "legend"),
          lapply(seq_along(legends), function(j) tags$img(src = legends[j], alt = paste(title, "legend", j))))))),
      tags$button(type = "button", class = "figure-enlarge", `data-enlarge-figure` = "", hidden = NA, "Enlarge figure"),
      tags$figcaption(id = paste0("figure-caption-", number), tags$strong(paste0("Figure ", number, ". ")), description)))
  }
}
report_details_start <- function(title) report_emit(htmltools::HTML(paste0("<details class=\"report-details\"><summary>", htmltools::htmlEscape(title), "</summary>")))
report_details_end <- function() cat("\n</details>\n\n")
