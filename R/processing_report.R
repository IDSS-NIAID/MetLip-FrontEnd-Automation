# Portable report snapshots and Quarto rendering. No Shiny state is read here.
REPORT_TEMPLATE_VERSION <- "0.13.1"

report_quarto_locations <- function() {
  if (.Platform$OS.type == "windows") {
    programs <- unique(Sys.getenv(c("ProgramFiles", "ProgramFiles(x86)", "LOCALAPPDATA"), unset = ""))
    programs <- programs[nzchar(programs)]
    locations <- unlist(lapply(programs, function(root) c(
      file.path(root, "Quarto", "bin", "quarto.exe"),
      file.path(root, "Programs", "Quarto", "bin", "quarto.exe"),
      file.path(root, "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe"),
      file.path(root, "RStudio", "resources", "app", "quarto", "bin", "quarto.exe"),
      file.path(root, "RStudio", "bin", "quarto", "bin", "quarto.exe"))), use.names = FALSE)
    locations <- c(locations, sub("\\.exe$", ".cmd", locations))
  } else {
    locations <- c("/usr/local/bin/quarto", "/usr/bin/quarto", "/opt/quarto/bin/quarto",
      "/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto",
      "/usr/lib/rstudio/resources/app/quarto/bin/quarto", "/usr/lib/rstudio/bin/quarto/bin/quarto",
      "/usr/lib/rstudio-server/bin/quarto/bin/quarto")
    if (identical(Sys.info()[["sysname"]], "Darwin")) locations <- c(
      "/Applications/quarto/bin/quarto", "/opt/homebrew/bin/quarto", locations,
      "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
      "/Applications/RStudio.app/Contents/MacOS/quarto/bin/quarto",
      path.expand("~/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"))
  }
  unique(locations)
}

find_report_quarto <- function(quarto_path = Sys.getenv("QUARTO_PATH", unset = ""),
                               path_binary = unname(Sys.which("quarto")),
                               rstudio_quarto = Sys.getenv("RSTUDIO_QUARTO", unset = ""),
                               candidates = report_quarto_locations()) {
  usable <- function(path) is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    file.exists(path.expand(path)) && !dir.exists(path.expand(path)) && file.access(path.expand(path), 1) == 0
  found <- function(path, source) list(path = normalizePath(path.expand(path), winslash = "/", mustWork = TRUE),
    source = source, message = "")
  # Never silently substitute another installation for an explicit user choice.
  if (is.character(quarto_path) && length(quarto_path) == 1L && !is.na(quarto_path) && nzchar(quarto_path)) {
    if (usable(quarto_path)) return(found(quarto_path, "QUARTO_PATH"))
    return(list(path = NULL, source = "QUARTO_PATH", message = paste0(
      "QUARTO_PATH does not point to an executable Quarto file: ", quarto_path,
      ". Correct it with Sys.setenv(QUARTO_PATH = '/full/path/to/quarto') or clear it with Sys.unsetenv('QUARTO_PATH') and recheck.")))
  }
  if (usable(path_binary)) return(found(path_binary, "PATH"))
  executable <- if (.Platform$OS.type == "windows") c("quarto.exe", "quarto.cmd") else "quarto"
  if (is.character(rstudio_quarto) && length(rstudio_quarto) == 1L && !is.na(rstudio_quarto) && nzchar(rstudio_quarto)) {
    for (path in c(rstudio_quarto, file.path(rstudio_quarto, executable)))
      if (usable(path)) return(found(path, "RSTUDIO_QUARTO"))
  }
  for (path in candidates) if (usable(path)) return(found(path, "installation"))
  list(path = NULL, source = "", message = paste0(
    "Quarto CLI was not found. Install Quarto from https://quarto.org/docs/get-started/ and restart R, ",
    "or set QUARTO_PATH to an existing Quarto executable and recheck. Installing or loading the quarto R package alone does not install the CLI."))
}

report_requirements <- function(find_quarto = find_report_quarto,
                                package_available = function(package) requireNamespace(package, quietly = TRUE),
                                run_version = function(binary) system2(binary, "--version", stdout = TRUE, stderr = TRUE, timeout = 10)) {
  tryCatch({
    packages <- c("quarto", "knitr", "rmarkdown", "htmltools")
    missing <- packages[!vapply(packages, package_available, logical(1))]
    if (length(missing)) return(list(ready = FALSE,
      message = paste0("Install report packages first: ", paste(missing, collapse = ", "),
        ". Install these suggested dependencies for MetLipAutomation; the Quarto CLI is also required.")))
    found <- find_quarto()
    if (is.null(found$path)) return(list(ready = FALSE, message = found$message))
    output <- suppressWarnings(run_version(found$path))
    status <- attr(output, "status")
    version <- trimws(output)
    version <- version[grepl("^[0-9]+\\.[0-9]+\\.[0-9]+([.-][[:alnum:].-]+)?$", version)]
    if ((!is.null(status) && status != 0) || length(version) != 1L)
      return(list(ready = FALSE, message = paste0("Quarto CLI was found at ", found$path,
        " but could not report its version. Check that this installation runs with --version, or correct QUARTO_PATH."), path = found$path))
    if (!identical(found$source, "QUARTO_PATH")) Sys.setenv(QUARTO_PATH = found$path)
    list(ready = TRUE, message = paste0("Quarto ", version, " is ready for HTML reports."), path = found$path, version = version)
  }, error = function(e) list(ready = FALSE,
    message = paste("Could not check Quarto report requirements:", conditionMessage(e))))
}

report_text <- function(value, fallback = "Not recorded") {
  if (is.null(value) || !length(value) || all(is.na(value))) return(fallback)
  paste(as.character(value), collapse = "\n")
}

report_plain_table <- function(value) {
  if (is.null(value)) return(NULL)
  if (!is.data.frame(value)) stop("A report table must be a data.frame.")
  result <- as.data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(result) <- NULL
  for (column in names(result)) {
    x <- result[[column]]
    if (is.factor(x) || inherits(x, c("Date", "POSIXt"))) x <- as.character(x)
    if (!is.atomic(x) || !is.null(dim(x))) stop("Report tables require ordinary atomic columns.")
    attributes(x) <- NULL
    result[[column]] <- x
  }
  result
}

report_setting_values <- function(settings) {
  if (!is.list(settings) || is.null(names(settings))) stop("Report settings must be a named list.")
  values <- vapply(settings, function(x) {
    if (is.null(x)) return("auto")
    if (!is.atomic(x)) stop("Report settings must contain ordinary scalar values.")
    paste(as.character(x), collapse = "; ")
  }, character(1))
  data.frame(Setting = names(settings), Value = unname(values), stringsAsFactors = FALSE)
}

report_plot_catalog <- function(result) {
  # Older result objects can still carry the retired diagnostic.
  keys <- setdiff(names(result$plots), "factors")
  if (is.null(keys)) keys <- character()
  if ("qc_rsd" %in% keys) {
    method <- result$selection$method
    evidence <- if (length(method) == 1L && !is.na(method)) result$comparison$evidence[[method]] else NULL
    # Match the finite observed RSD values used by the QC precision plot.
    # Zero RSD is valid; missing evidence and insufficient QCs omit the figure.
    if (!any(is.finite(evidence$qc$RSD_percent))) keys <- setdiff(keys, "qc_rsd")
  }
  titles <- c(intensity_threshold_sensitivity = "Intensity threshold sensitivity",
    per_sample_detection_counts = "Detection by sample", per_lipid_missingness = "Missingness by lipid",
    density = "Intensity distributions", rle = "Relative log intensity (RLE)", distribution = "Sample intensity summaries",
    qc_rsd = "QC precision",
    pca_scores = "PCA sample scores", pca_loadings = "PCA lipid loadings", pca_scree = "PCA explained variance",
    pca_biplot = "PCA biplot", pca_pairs = "PCA scores pairs")
  if (is.list(result$selected_pca) && !inherits(result$selected_pca, "error")) titles["pca_scree"] <- paste("PCA", tolower(pca_variance_label(result$selected_pca)))
  labels <- unname(titles[keys])
  labels[is.na(labels)] <- gsub("_", " ", keys[is.na(labels)])
  descriptions <- c(
    intensity_threshold_sensitivity = "Study samples before detection filtering and imputation. The denominator is nonmissing peak-area measurements; original missing values are excluded. The vertical line marks the recorded intensity threshold in input peak-area units.",
    per_sample_detection_counts = "Counts across cleaned features before study detection filtering and imputation, including study samples and controls. Detected values meet the intensity threshold; below-threshold and originally missing measurements are counted separately.",
    per_lipid_missingness = "Study samples before the detection-proportion filter and imputation. Missingness here combines original missing values and values below the intensity threshold. Features with no study detections are already excluded. The dashed 25% and 50% lines are hypothetical reference cutoffs; the solid line is the applied cutoff.",
    density = "Study samples and eligible QCs after the selected sample normalization, including shared imputation. Density is shown against log2 normalized peak area, independently of the PCA transformation. Curves integrate to one and do not measure total signal; low-intensity structure can reflect imputation.",
    rle = "Study samples and eligible QCs after the selected normalization, including shared imputation. Values are log2 intensities minus the feature-wise median log2 intensity of study samples. Distribution alignment alone does not establish preservation of biological differences.",
    distribution = "Study samples and eligible QCs after the selected normalization, including shared imputation. The display uses log2 peak areas independently of the PCA transformation. Offsets can reflect technical or biological variation.",
    qc_rsd = "Eligible QC observations after normalization and before transformation, centering or scaling. RSD (%) is 100 times the sample standard deviation divided by the mean. Imputed entries do not count as repeated observations, although sample factors use the shared imputed matrix. The curve summarizes eligible feature/pool/batch combinations; the vertical line is the recorded descriptive threshold. Precision does not establish accuracy or overall study validity.",
    pca_scores = "Sample scores from the recorded PCA fit, with the selected transformation, centering, scaling and QC scope. Coordinates are in component units of the analysis matrix. Apparent separation is exploratory and is not a statistical significance test.",
    pca_loadings = "Feature rotation coefficients from the recorded PCA fit. These are dimensionless weights, not feature-PC correlations, effect sizes or significance measures. Displayed labels can be a subset of the complete loading table.",
    pca_scree = "Individual and cumulative component percentages from the recorded fit. The denominator includes the full singular-value spectrum. The recorded variance basis distinguishes centered variance from uncentered sums of squares.",
    pca_biplot = "Sample positions are unchanged PCA scores. Feature arrows use rotation coefficients on separate loading axes; arrow selection changes the display only. Distances between samples and arrow tips do not represent a shared measurement scale.",
    pca_pairs = "Sample scores across the displayed component pairs. Columns identify x components and rows identify y components. Diagonal percentages use the recorded variance basis; no correlation tests are performed.")
  explanations <- unname(descriptions[keys])
  explanations[is.na(explanations)] <- "Additional recorded figure. Its axes and original caption identify the measurement and scope."
  data.frame(key = keys, title = analysis_text(labels, result),
    explanation = explanations,
    stringsAsFactors = FALSE)
}

# Availability is recorded independently of whether a figure was requested.
# No thresholds or quality grades are introduced by the report.
report_qc_assessment <- function(result) {
  state <- function(status, reason) list(status = status, reason = reason)
  if (identical(result$settings$processing_mode, "skip"))
    return(state("Not run", "Prepared-data mode does not calculate normalization-comparison QC precision."))
  method <- result$selection$method
  if (is.null(result$comparison) || length(method) != 1L)
    return(state("Not recorded", "Normalization-comparison evidence or the selected method was not recorded."))
  evidence <- result$comparison$evidence[[method]]
  if (is.null(evidence)) return(state("Not recorded", "QC evidence for the selected method was not recorded."))
  if (any(is.finite(evidence$qc$RSD_percent)))
    return(state("Available", "Observed QC precision is available for the selected normalization."))
  n <- length(result$comparison$input$qc)
  reason <- if (!n) "No QC samples were identified in the comparison input." else
    paste0("No finite observed QC RSD values meet the recorded eligibility rules (", n,
      " identified QC samples). Sample detection, feature detection and the minimum observations per pool/batch determine eligibility.")
  state("Not assessable", reason)
}

report_pca_snapshot <- function(result) {
  fit <- result$selected_pca
  if (is.null(fit) || inherits(fit, "error")) return(list(available = FALSE,
    message = if (inherits(fit, "error")) conditionMessage(fit) else "PCA was not available for this workflow."))
  scores <- report_plain_table(pca_scores_export(fit))
  loadings <- report_plain_table(pca_loadings_export(fit))
  preview <- function(x, priority = character()) {
    columns <- c(intersect(priority, names(x)), setdiff(names(x), priority))
    x[seq_len(min(20L, nrow(x))), utils::head(columns, 9L), drop = FALSE]
  }
  list(available = TRUE, variance_label = pca_variance_label(fit), settings = report_setting_values(fit$settings),
    plot_settings = if (length(result$pca_view)) report_setting_values(result$pca_view) else
      data.frame(Setting = character(), Value = character()),
    plot_errors = result$pca_plot_errors,
    variance = report_plain_table(pca_variance_table(fit)),
    scores_preview = preview(scores, c("Acquired_Sample_ID", "metadata__.Role", "metadata__.Imputed_fraction")),
    loadings_preview = preview(loadings, c("Lipid", "Metabolite", "PC1", "PC2",
      paste0("Pathway: ", c(result$pca_view$feature_color, result$pca_view$feature_shape)))),
    score_dimensions = c(rows = nrow(scores), columns = ncol(scores)),
    loading_dimensions = c(rows = nrow(loadings), columns = ncol(loadings)),
    omitted_features = report_plain_table(export_feature_table(fit$removed_features, result, columns = "Lipid")))
}

write_report_source <- function(result, directory, template_path = metlip_report_template()) {
  if (!is.list(result)) stop("A completed result list is required for reporting.")
  terms <- analysis_terms(result)
  required <- c("summary", "settings", "excluded_samples", "normalized_wide", "plots", "plot_sizes")
  missing <- setdiff(required, names(result))
  if (length(missing)) stop("Report result is missing: ", paste(missing, collapse = ", "))
  if (!is.character(template_path) || length(template_path) != 1L || !file.exists(template_path))
    stop("The bundled Quarto report template is missing.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("The ggplot2 package is required to create report figures.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Cannot create the report directory.")
  directory <- normalizePath(directory, mustWork = TRUE)
  figure_directory <- file.path(directory, "figures")
  dir.create(figure_directory, showWarnings = FALSE)
  catalog <- report_plot_catalog(result)
  unlink(file.path(figure_directory, "factors.png"))
  if (!"qc_rsd" %in% catalog$key) unlink(file.path(figure_directory, "qc_rsd.png"))
  catalog$path <- if (nrow(catalog)) paste0("figures/", analysis_text(catalog$key, terms$id), ".png") else character()
  catalog$display_width <- integer(nrow(catalog))
  catalog$legend_paths <- vector("list", nrow(catalog))
  for (i in seq_len(nrow(catalog))) {
    key <- catalog$key[i]
    size <- result$plot_sizes[[key]]
    if (is.null(size) || length(size$width) != 1L || length(size$height) != 1L ||
        any(!is.finite(c(size$width, size$height))) || any(c(size$width, size$height) <= 0))
      stop("Invalid report plot dimensions for ", key, ".")
    plot <- result$plots[[key]]
    # Report-only wording: preserve the original plots and all numerical exports.
    if (key %in% c("rle", "distribution")) plot <- plot + ggplot2::labs(
      caption = "Offsets may be technical or biological. Distribution alignment alone does not establish biological preservation.")
    if (key == "rle") plot <- plot + ggplot2::labs(title = "Relative log intensity (RLE)")
    if (key == "qc_rsd" && (nzchar(result$comparison$config$pool) || nzchar(result$comparison$config$batch)))
      plot <- plot + ggplot2::labs(y = "Fraction of eligible feature/pool/batch combinations")
    if (grepl("^pca_", key)) {
      parts <- plot_legend_parts(plot)
      plot <- parts$plot
      catalog$legend_paths[[i]] <- paste0("figures/", write_legend_images(parts,
        figure_directory, analysis_text(key, terms$id)))
      if (is.null(parts$legend)) catalog$legend_paths[[i]] <- character()
    }
    ggplot2::ggsave(file.path(directory, catalog$path[i]), plot = plot,
      device = "png", width = size$width, height = size$height, units = "in",
      dpi = 110, bg = "white", limitsize = FALSE)
    catalog$display_width[i] <- as.integer(max(800, round(size$width * 85)))
  }

  wide <- result$normalized_wide
  supplied_preview <- is.null(wide)
  if (supplied_preview && !is.null(result$supplied_study)) wide <- data.frame(
    Lipid = colnames(result$supplied_study), t(result$supplied_study), check.names = FALSE, row.names = NULL)
  wide <- report_plain_table(export_feature_table(wide, terms$id, columns = "Lipid"))
  preview <- wide[seq_len(min(20L, nrow(wide))), seq_len(min(9L, ncol(wide))), drop = FALSE]
  # Deliberately copy only plain tables and scalar metadata. A full result can
  # contain live upload paths in its signature and environments inside ggplots.
  payload <- list(
    schema_version = 5L,
    processing_skipped = identical(result$settings$processing_mode, "skip"),
    supplied_preview = supplied_preview,
    analysis_error = result$analysis_error,
    qc_eligibility = report_plain_table(result$qc_eligibility),
    analysis = terms,
    selection = report_plain_table(settings_table(result$selection)),
    comparison_scorecard = report_plain_table(comparison_scorecard_view(result$comparison)),
    evaluation_settings = if (is.null(result$comparison)) NULL else report_plain_table(settings_table(result$comparison$config[c("reference", "rsd_limit", "detection", "min_qc")])),
    evaluation_context = result$comparison$config,
    qc_assessment = report_qc_assessment(result),
    data_index = if (is.null(result$selection)) NULL else report_plain_table(result_table_index(result_table_catalog(result))),
    scaling_scorecard = report_plain_table(result$scaling_scorecard),
    template_version = REPORT_TEMPLATE_VERSION,
    app_version = report_text(result$app_version),
    project_title = report_text(result$project_title, terms$default_title),
    author = report_text(result$author, ""),
    metlip_id = report_text(result$metlip_id, ""),
    tas_id = report_text(result$tas_id, ""),
    completed_at = report_text(result$completed_at),
    summary = report_plain_table(result$summary),
    settings = report_setting_values(active_processing_settings(result)),
    excluded_samples = as.character(result$excluded_samples),
    duplicate_audit = report_plain_table(result_duplicate_audit(result)),
    duplicate_review_summary = report_plain_table(duplicate_audit_summary(result_duplicate_audit(result))),
    presence_filter_status = report_text(result_presence_filter(result)$status, "not_recorded"),
    presence_filter_summary = report_plain_table(presence_filter_summary(result)),
    presence_filter_audit = report_plain_table(export_feature_table(result_presence_filter_audit(result),
      terms$id, columns = c("Lipid", "Lipid_original"))),
    presence_filter_unmatched_identifiers = as.character(result_presence_filter(result)$unmatched_approved_identifiers),
    sample_roles = report_plain_table(result$sample_roles),
    unused_metadata = as.character(result$unused_metadata),
    provenance = report_plain_table(result_input_provenance(result)),
    pathway_summary = report_plain_table(pathway_summary(result$pathway_annotation)),
    pathway_settings = report_plain_table(pathway_tables(result$pathway_annotation)$pathway_settings),
    normalization_factors = report_plain_table(result$normalization_factors),
    missingness = report_plain_table(export_feature_table(result$missingness, terms$id, columns = "Lipid")),
    detection_counts = report_plain_table(result$detection_counts),
    normalized_preview = preview,
    normalized_dimensions = c(features = nrow(wide), samples = max(0L, ncol(wide) - 1L)),
    pca = report_pca_snapshot(result),
    plots = catalog,
    density_bandwidth = if (is.numeric(result$density_bandwidth) && length(result$density_bandwidth) == 1L &&
      is.finite(result$density_bandwidth)) unname(result$density_bandwidth) else NULL,
    messages = as.character(result$messages),
    analysis_session_info = report_text(result$analysis_session_info,
      "Analysis-time R session information was not recorded for this result."))
  # Explicit plain-data validation protects portability if a new field is added.
  portable <- function(x) {
    if (is.null(x)) return(TRUE)
    if (is.data.frame(x)) return(all(vapply(x, portable, logical(1))))
    if (is.list(x)) return(all(vapply(x, portable, logical(1))))
    is.atomic(x) && !is.object(x)
  }
  if (!portable(payload)) stop("Report snapshot contains a nonportable object.")
  saveRDS(payload, file.path(directory, "report_data.rds"), version = 3)
  template_target <- file.path(directory, "report.qmd")
  template <- readLines(template_path, warn = FALSE)
  # Only the fixed document title changes; no user text is substituted into source.
  title_line <- which(grepl("^title:", template))[1L]
  if (is.na(title_line)) stop("The report template is missing its title.")
  template[title_line] <- paste0('title: "', terms$label, ' processing report"')
  # Persist the default HTML name in portable sources as well as browser downloads.
  html_name <- metlip_download_name(paste0(terms$file_prefix, "_report.html"), result$metlip_id)
  template <- append(template, paste0('output-file: "', html_name, '"'), after = title_line)
  writeLines(template, template_target)
  # These assets are report-only; Shiny's existing plot styles are untouched.
  for (asset in c("report-layout.css", "report_helpers.R", "report.js")) {
    if (!file.copy(file.path(dirname(template_path), asset), file.path(directory, asset), overwrite = TRUE))
      stop("Could not prepare report asset: ", asset)
  }
  writeLines(c(
    paste(terms$label, "report source"), "",
    "This folder re-renders the completed report from a fixed data snapshot and figure images.",
    "It does not rerun cleaning, filtering, imputation or normalization.",
    "Keep report.qmd, report_data.rds, report_helpers.R, report-layout.css, report.js and the figures folder together.",
    "With R, knitr, rmarkdown, htmltools and Quarto CLI installed, run:",
    "  quarto render report.qmd --to html", "",
    "The report includes bounded data/PCA previews. Download tables or the full results ZIP for complete matrices and audit tables.",
    "Reproducing the analysis itself also requires the original inputs, processing code and matching software versions."
  ), file.path(directory, "REPORT_SOURCE_README.txt"))
  invisible(template_target)
}

render_report <- function(source_dir, output_name = NULL) {
  requirements <- report_requirements()
  if (!isTRUE(requirements$ready)) stop(requirements$message, call. = FALSE)
  source_dir <- normalizePath(source_dir, mustWork = TRUE)
  qmd <- file.path(source_dir, "report.qmd")
  if (!all(file.exists(c(qmd, file.path(source_dir, "report_data.rds")))))
    stop("Report source or data snapshot is missing.")
  if (is.null(output_name)) {
    snapshot <- readRDS(file.path(source_dir, "report_data.rds"))
    prefix <- if (identical(snapshot$analysis$id, "metabolomics")) "metabolomics" else "lipidomics"
    output_name <- metlip_download_name(paste0(prefix, "_report.html"), snapshot$metlip_id)
  }
  if (!is.character(output_name) || length(output_name) != 1L || is.na(output_name) ||
      output_name != basename(output_name) || grepl("[/\\\\]", output_name) ||
      !grepl("\\.html$", output_name, ignore.case = TRUE))
    stop("Report output must be a single HTML filename without a directory.")
  output <- file.path(source_dir, output_name)
  # A failed render must never be mistaken for an older successful report.
  if (file.exists(output)) unlink(output)
  rendered <- FALSE
  on.exit(if (!rendered) unlink(output), add = TRUE)
  tryCatch(quarto::quarto_render(input = qmd, output_format = "html", output_file = output_name,
    execute_params = list(snapshot = "report_data.rds"), execute_dir = source_dir,
    as_job = FALSE, cache = FALSE, quiet = TRUE),
    error = function(e) stop("Quarto report rendering failed: ", conditionMessage(e), call. = FALSE))
  if (!file.exists(output) || is.na(file.info(output)$size) || file.info(output)$size <= 0)
    stop("Quarto did not produce the requested HTML report.")
  output <- normalizePath(output, mustWork = TRUE)
  rendered <- TRUE
  output
}
