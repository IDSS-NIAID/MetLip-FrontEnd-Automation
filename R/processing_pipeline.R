# Shared metabolomics/lipidomics processing core. No file writes or Shiny dependencies.
# Derived from the supplied processing script; the bundled tsn_long is unchanged.
# Internal package helpers; dependencies are qualified or explicitly imported.

analysis_terms <- function(x = NULL) {
  mode <- if (is.character(x)) x else if (is.list(x)) {
    if (!is.null(x$data_type)) x$data_type else if (is.character(x$id) && length(x$id) == 1L && !is.na(x$id) &&
      x$id %in% c("lipidomics", "metabolomics")) x$id else if (!is.null(x$settings$data_type)) x$settings$data_type else
      if (!is.null(x$processing_settings$data_type)) x$processing_settings$data_type else x$input$data_type
  } else NULL
  if (is.null(mode)) mode <- "lipidomics"
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) || !mode %in% c("lipidomics", "metabolomics"))
    stop("Data type must be lipidomics or metabolomics.")
  metabolomics <- mode == "metabolomics"
  title <- if (metabolomics) "Metabolomics" else "Lipidomics"
  code <- if (metabolomics) "TCM" else "TBL"
  label <- paste0(title, " (", code, ")")
  list(id = mode, label = label, title = title, code = code,
    feature = if (metabolomics) "metabolite" else "lipid",
    features = if (metabolomics) "metabolites" else "lipids",
    column = if (metabolomics) "Metabolite" else "Lipid", file_prefix = mode,
    default_title = paste(label, "processing & QC"))
}

# Call only on application-authored wording, never identifiers or user text.
analysis_text <- function(text, x = NULL) {
  if (analysis_terms(x)$id == "lipidomics") return(text)
  replacements <- c(Lipidomics = "Metabolomics", lipidomics = "metabolomics",
    Lipids = "Metabolites", lipids = "metabolites", Lipid = "Metabolite", lipid = "metabolite")
  for (word in names(replacements)) text <- gsub(paste0("(?<![[:alpha:]])", word, "(?![[:alpha:]])"),
    replacements[[word]], text, perl = TRUE)
  text
}

# Select known identifier columns at the call site; wide matrix headers may be sample IDs.
export_feature_table <- function(table, x = NULL, columns = c("Lipid", "Lipid_original")) {
  if (analysis_terms(x)$id == "lipidomics") return(table)
  columns <- intersect(columns, c("Lipid", "Lipid_original", "Lipid_base"))
  for (column in columns) for (index in which(names(table) == column)) {
    label <- sub("^Lipid", "Metabolite", column)
    # Preserve user sample/metadata headers when they match the generated label.
    existing <- names(table)[-index]
    if (label %in% existing) label <- utils::tail(make.unique(c(existing, paste0(label, "_ID")), sep = "_"), 1L)
    names(table)[index] <- label
  }
  table
}

default_settings <- function() {
  list(data_type = "lipidomics", processing_mode = "process", intensity_threshold = 1e4, min_presence = 0.5, impute_fraction = 0.2,
       input_delimiter = "auto", metadata_delimiter = "auto", orientation = "auto",
       sample_name_column = NULL, metadata_name_column = NULL,
       blank_pattern = "^Blank(_[0-9]+)?(_batch_[0-9]+)?$",
       qc_pattern = "^QC(_[0-9]+)?(_batch_[0-9]+)?$", density_sample_alpha = 0.15,
       density_sample_linewidth = 0.5, density_mean_linewidth = 1.3)
}

validate_settings <- function(settings) {
  unknown <- setdiff(names(settings), names(default_settings()))
  if (length(unknown)) stop("Unknown setting(s): ", paste(unknown, collapse = ", "))
  settings <- utils::modifyList(default_settings(), settings, keep.null = TRUE)
  if (!is.character(settings$processing_mode) || length(settings$processing_mode) != 1L ||
      is.na(settings$processing_mode) || !settings$processing_mode %in% c("process", "skip"))
    stop("Processing mode must be process or skip.")
  if (settings$processing_mode == "skip") {
    inactive <- c("intensity_threshold", "min_presence", "impute_fraction",
      "density_sample_alpha", "density_sample_linewidth", "density_mean_linewidth")
    settings[inactive] <- default_settings()[inactive]
  }
  if (is.null(settings$data_type)) stop("Data type must be lipidomics or metabolomics.")
  settings$data_type <- analysis_terms(settings$data_type)$id
  for (name in c("intensity_threshold", "min_presence", "impute_fraction",
                 "density_sample_alpha", "density_sample_linewidth", "density_mean_linewidth")) {
    value <- settings[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value))
      stop(name, " must be one finite number.")
  }
  if (settings$intensity_threshold <= 0) stop("Intensity threshold must be positive.")
  if (settings$min_presence < 0 || settings$min_presence > 1) stop("Detection proportion must be between 0 and 1.")
  if (settings$impute_fraction <= 0 || settings$impute_fraction > 1) stop("Imputation fraction must be greater than 0 and at most 1.")
  if (settings$density_sample_alpha < 0 || settings$density_sample_alpha > 1)
    stop("Density sample alpha must be between 0 and 1.")
  if (settings$density_sample_linewidth <= 0 || settings$density_mean_linewidth <= 0)
    stop("Density line widths must be positive.")
  for (name in c("input_delimiter", "metadata_delimiter")) {
    value <- settings[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% c("auto", ",", "\t"))
      stop(name, " must be auto, a comma, or a tab.")
  }
  if (length(settings$orientation) != 1L || is.na(settings$orientation) ||
      !settings$orientation %in% c("auto", "lipids_in_rows", "samples_in_rows"))
    stop("Orientation must be auto, lipids_in_rows, or samples_in_rows.")
  for (name in c("sample_name_column", "metadata_name_column")) {
    value <- settings[[name]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value) || trimws(value) == ""))
      stop(name, " must be NULL or one nonempty column name.")
  }
  for (name in c("blank_pattern", "qc_pattern")) {
    value <- settings[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
      stop(name, " must be one nonempty regular expression.")
    tryCatch(withCallingHandlers(grepl(value, "test", ignore.case = TRUE), warning = function(w) stop(w)),
             error = function(e) stop("Invalid ", name, ": ", conditionMessage(e)))
  }
  settings
}

count_table_fields <- function(lines, separator) {
  connection <- textConnection(lines)
  on.exit(close(connection))
  counts <- utils::count.fields(connection, sep = separator, quote = "\"", comment.char = "", blank.lines.skip = FALSE)
  return(counts)
}

read_checked_table <- function(file, delimiter = "auto", min_columns = 2L,
                               preserve_original_headers = FALSE) {
  if (length(delimiter) != 1L || is.na(delimiter) || !delimiter %in% c("auto", ",", "\t")) stop("Delimiter must be auto, a comma, or a tab.")
  lines <- withCallingHandlers(readLines(file, warn = FALSE), warning = function(w) stop(paste0("Cannot read ", basename(file), ": ", conditionMessage(w)), call. = FALSE))
  if (length(lines) < 2L) stop(paste0(basename(file), " must contain a header and at least one data row."))
  lines[1L] <- sub("^\ufeff", "", lines[1L])
  if (any(trimws(lines) == "")) stop(paste0(basename(file), " contains blank records. Remove blank lines before importing."))

  if (delimiter == "auto") {
    header_counts <- vapply(c("\t", ","), function(separator) count_table_fields(lines[1L], separator)[1L], integer(1))
    candidates <- which(!is.na(header_counts) & header_counts > 1L)
    if (min_columns == 1L && !length(candidates) && isTRUE(all(header_counts == 1L))) {
      # ID-only metadata has no separator to detect; record checks still apply.
      delimiter <- ","
    } else {
      if (length(candidates) != 1L) stop(paste0("Cannot identify one delimiter for ", basename(file), ". Check the header or set its delimiter explicitly to a comma or tab."))
      delimiter <- c("\t", ",")[candidates]
    }
  }

  field_counts <- withCallingHandlers(count_table_fields(lines, delimiter), warning = function(w) stop(paste0("Malformed input in ", basename(file), ": ", conditionMessage(w)), call. = FALSE))
  if (anyNA(field_counts) || length(field_counts) != length(lines)) stop(paste0(basename(file), " contains incomplete quotes or quoted fields spanning lines. Export one complete record per line."))
  if (field_counts[1L] < min_columns) stop(paste0(basename(file), " has only one column. Check the delimiter and header."))
  bad_lines <- which(field_counts != field_counts[1L])
  if (length(bad_lines) > 0L) stop(paste0(basename(file), " has inconsistent field counts at file line(s): ", paste(utils::head(bad_lines, 10L), collapse = ", "), ". Every row must have the same number of fields as the header."))

  result <- withCallingHandlers(utils::read.table(text = lines, header = TRUE, sep = delimiter, quote = "\"", comment.char = "", check.names = FALSE, colClasses = "character", na.strings = character(0), fill = FALSE, row.names = NULL, blank.lines.skip = FALSE), warning = function(w) stop(paste0("Import warning for ", basename(file), ": ", conditionMessage(w)), call. = FALSE))
  if (ncol(result) != field_counts[1L] || nrow(result) != length(lines) - 1L) stop(paste0("Imported dimensions do not match the records in ", basename(file), ". Check quoting and delimiters."))
  names(result) <- trimws(names(result))
  if (isTRUE(preserve_original_headers)) {
    # Reading a header as a header strips unquoted surrounding whitespace even
    # with check.names = FALSE. Read the checked first record as character data
    # so notes can match exact feature identifiers in samples-in-rows inputs.
    header <- utils::read.table(text = lines[1L], header = FALSE, sep = delimiter, quote = "\"",
      comment.char = "", colClasses = "character", na.strings = character(0),
      fill = FALSE, row.names = NULL, strip.white = FALSE, check.names = FALSE)
    attr(result, "original_headers") <- as.character(header[1L, ])
  }
  message("Imported ", basename(file), ": ", nrow(result), " rows x ", ncol(result), " columns; separator = ", if (delimiter == "\t") "tab" else "comma", ".")
  return(result)
}

# normalize label spelling only; sample IDs themselves are never case-folded
normalize_label <- function(x) {
  return(gsub("[^a-z0-9]", "", tolower(trimws(x))))
}



read_metadata_table <- function(path, original_name, delimiter, sheet = 1) {
  extension <- tolower(tools::file_ext(original_name))
  if (extension %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("The readxl package is required for Excel metadata.")
    # Explicit readers use the original filename, even when Shiny's path has no extension.
    reader <- if (extension == "xlsx") readxl::read_xlsx else readxl::read_xls
    result <- as.data.frame(reader(path, sheet = sheet, col_types = "text",
                                   trim_ws = FALSE, .name_repair = "minimal"),
                            check.names = FALSE, stringsAsFactors = FALSE)
    if (nrow(result) == 0L || ncol(result) < 1L) stop("The metadata sheet needs headers and at least one sample row.")
    names(result) <- trimws(names(result))
    message("Imported Excel metadata: ", nrow(result), " rows x ", ncol(result), " columns.")
    return(result)
  }
  if (!extension %in% c("csv", "txt", "tsv")) stop("Metadata must be CSV, TXT, TSV, XLSX, or XLS.")
  read_checked_table(path, delimiter, min_columns = 1L)
}

empty_duplicate_occurrences <- function() {
  data.frame(Occurrence_key = character(), Acquired_Sample_ID = character(),
    Source = character(), Filename = character(), Sheet = character(), Axis = character(),
    Position = integer(), Index = integer(), stringsAsFactors = FALSE)
}

empty_duplicate_resolutions <- function() {
  data.frame(Acquired_Sample_ID = character(), Action = character(),
    Peak_occurrence = character(), Metadata_occurrence = character(), stringsAsFactors = FALSE)
}

measurement_number_pattern <- function() "^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$"

empty_duplicate_signal <- function() {
  data.frame(Occurrence_key = character(), Acquired_Sample_ID = character(),
    Filename = character(), Axis = character(), Position = integer(),
    Total_input_signal = numeric(), Measured_features = integer(), Missing_features = integer(),
    Negative_features = integer(), Invalid_features = integer(), Signal_status = character(),
    stringsAsFactors = FALSE)
}

# Compare the originally uploaded measurements before feature filtering,
# imputation, standard removal, or normalization. Source positions distinguish
# occurrences sharing an ID. Missing cells contribute nothing; negatives use
# the same zero floor as processing. Invalid input never produces a partial sum.
duplicate_signal_summary <- function(inspected) {
  occurrences <- inspected$duplicate_occurrences
  if (is.null(occurrences) || !nrow(occurrences)) return(empty_duplicate_signal())
  occurrences <- occurrences[occurrences$Source == "Peak-area data", , drop = FALSE]
  if (!nrow(occurrences)) return(empty_duplicate_signal())
  dat <- inspected$duplicate_original$data
  if (!is.data.frame(dat) || anyNA(occurrences$Index) ||
      any(!occurrences$Index %in% seq.int(2L, ncol(dat))))
    stop("Original measurement occurrences are unavailable. Review the input files again.")
  out <- occurrences[, c("Occurrence_key", "Acquired_Sample_ID", "Filename", "Axis", "Position"), drop = FALSE]
  rownames(out) <- NULL
  out$Total_input_signal <- rep(NA_real_, nrow(out))
  for (field in c("Measured_features", "Missing_features", "Negative_features", "Invalid_features"))
    out[[field]] <- integer(nrow(out))
  out$Signal_status <- rep("Ready", nrow(out))
  for (row in seq_len(nrow(out))) {
    text <- trimws(dat[[occurrences$Index[row]]])
    missing <- is.na(text) | text %in% c("", "NA", "N/A")
    values <- suppressWarnings(as.numeric(text))
    valid <- !missing & grepl(measurement_number_pattern(), text) & is.finite(values)
    invalid <- !missing & !valid
    out$Measured_features[row] <- as.integer(sum(valid))
    out$Missing_features[row] <- as.integer(sum(missing))
    out$Negative_features[row] <- as.integer(sum(values[valid] < 0))
    out$Invalid_features[row] <- as.integer(sum(invalid))
    if (any(invalid)) {
      out$Signal_status[row] <- "Invalid measurements"
    } else if (!any(valid)) {
      out$Signal_status[row] <- "No measured values"
    } else {
      total <- sum(pmax(values[valid], 0))
      if (is.finite(total)) out$Total_input_signal[row] <- total else
        out$Signal_status[row] <- "Total exceeds numeric range"
    }
  }
  out
}

# Fill only unambiguous measurement choices, retaining any manual metadata choice.
# Use original, full-precision review totals rather than their displayed rounding.
select_highest_duplicate_choices <- function(inspected, choices) {
  occurrences <- inspected$duplicate_occurrences
  signal <- inspected$duplicate_signal
  if (is.null(signal)) signal <- duplicate_signal_summary(inspected)
  selected_ids <- character()
  skipped <- data.frame(Acquired_Sample_ID = character(), Reason = character(),
    stringsAsFactors = FALSE)
  for (id in inspected$duplicate_ids) {
    peak <- occurrences[occurrences$Acquired_Sample_ID == id &
      occurrences$Source == "Peak-area data", , drop = FALSE]
    if (nrow(peak) < 2L) next
    totals <- signal$Total_input_signal[match(peak$Occurrence_key, signal$Occurrence_key)]
    reason <- if (any(!is.finite(totals))) "One or more totals are unavailable" else
      if (sum(totals == max(totals)) > 1L) "Highest total peak area is tied" else NULL
    if (!is.null(reason)) {
      skipped <- rbind(skipped, data.frame(Acquired_Sample_ID = id, Reason = reason,
        stringsAsFactors = FALSE))
      next
    }
    index <- match(id, choices$Acquired_Sample_ID)
    choices$Peak_occurrence[index] <- peak$Occurrence_key[which.max(totals)]
    metadata <- occurrences[occurrences$Acquired_Sample_ID == id &
      occurrences$Source == "Metadata", , drop = FALSE]
    if (nrow(metadata) == 1L) choices$Metadata_occurrence[index] <- metadata$Occurrence_key
    choices$Action[index] <- "keep"
    selected_ids <- c(selected_ids, id)
  }
  list(choices = choices, selected_ids = selected_ids, skipped = skipped)
}

# Append review evidence without assigning a measurement total to metadata rows.
attach_duplicate_signal <- function(occurrence_rows, signal = empty_duplicate_signal()) {
  if (is.null(signal)) signal <- empty_duplicate_signal()
  fields <- c("Total_input_signal", "Measured_features", "Missing_features", "Negative_features",
    "Invalid_features", "Signal_status")
  at <- match(occurrence_rows$Occurrence_key, signal$Occurrence_key)
  at[occurrence_rows$Source != "Peak-area data"] <- NA_integer_
  for (field in fields) occurrence_rows[[field]] <- signal[[field]][at]
  occurrence_rows$Signal_status[occurrence_rows$Source != "Peak-area data"] <- "Not applicable"
  occurrence_rows
}

empty_duplicate_audit <- function() {
  out <- attach_duplicate_signal(empty_duplicate_occurrences())
  out$Index <- NULL
  out$Status <- character()
  out$Decision <- character()
  out
}

empty_presence_filter_audit <- function() {
  data.frame(Source_feature_index = integer(), Source_axis = character(),
    Source_position = integer(), Lipid_original = character(), Lipid = character(),
    Signal_total = numeric(), Decision = character(), stringsAsFactors = FALSE)
}

empty_presence_filter <- function(settings) {
  list(status = if (analysis_terms(settings)$id == "metabolomics") "skipped" else "not_applicable",
    notes_name = NULL, notes_md5 = NULL, approved_identifiers = character(),
    unmatched_approved_identifiers = character(),
    counts = list(input_features = 0L, approved_features = 0L, excluded_not_approved = 0L,
      collapsed_duplicates = 0L, retained_features = 0L))
}

tcm_metabolite_names <- function(identifiers) {
  sub("_(1|2|3|pos|1pos)$", "", identifiers)
}

inspect_presence_notes <- function(path, name, feature_source, settings) {
  notes <- read_checked_table(path, delimiter = ",")
  if (sum(names(notes) == "Metabolite") != 1L || sum(names(notes) == "Present") != 1L)
    stop("Peak-picking notes must contain the columns Metabolite and Present exactly once each.")
  approved <- unique(notes$Metabolite[!is.na(notes$Present) & notes$Present == "Y"])
  if (anyNA(approved) || any(trimws(approved) %in% c("", "NA", "N/A")))
    stop("Peak-picking notes contain a missing or blank Metabolite identifier marked Y.")
  original <- feature_source$Lipid_original
  eligible <- !grepl("d7_SPLASH", original, fixed = TRUE)
  retained <- eligible & original %in% approved
  if (!any(retained)) stop("Peak-picking notes retain no nonstandard analysis features. Check exact Metabolite identifiers and uppercase Y entries.")
  final_names <- tcm_metabolite_names(original[retained])
  if (any(trimws(final_names) == "")) stop("Peak-picking suffix removal produced an empty metabolite name.")
  out <- empty_presence_filter(settings)
  out$status <- "applied"
  out$notes_name <- name
  out$notes_md5 <- unname(tools::md5sum(path))
  out$approved_identifiers <- approved
  out$unmatched_approved_identifiers <- setdiff(approved, original)
  out$counts <- list(input_features = as.integer(sum(eligible)),
    approved_features = as.integer(sum(retained)),
    excluded_not_approved = as.integer(sum(eligible & !retained)),
    collapsed_duplicates = as.integer(sum(retained) - length(unique(final_names))),
    retained_features = as.integer(length(unique(final_names))))
  message("Peak-picking notes: ", sum(retained), " approved analysis signals; ",
    out$counts$excluded_not_approved, " signals not approved.")
  if (length(out$unmatched_approved_identifiers)) message("Approved notes identifiers absent from the input: ",
    length(out$unmatched_approved_identifiers), ". Examples: ",
    paste(utils::head(out$unmatched_approved_identifiers, 10L), collapse = ", "))
  out
}

select_presence_signals <- function(dat, feature_source, presence_filter) {
  if (nrow(feature_source) != nrow(dat)) stop("Original feature positions changed after input review. Review the current inputs again.")
  audit <- feature_source
  approved <- audit$Lipid_original %in% presence_filter$approved_identifiers
  audit$Lipid <- rep(NA_character_, nrow(audit))
  audit$Lipid[approved] <- tcm_metabolite_names(audit$Lipid_original[approved])
  audit$Signal_total <- rep(NA_real_, nrow(audit))
  audit$Decision <- ifelse(approved, "duplicate_discarded", "not_approved")
  if (!any(approved)) stop("Peak-picking notes retain no nonstandard analysis features.")
  if (any(trimws(audit$Lipid[approved]) == "")) stop("Peak-picking suffix removal produced an empty metabolite name.")
  values <- as.matrix(dat[approved, -1L, drop = FALSE])
  value_text <- trimws(as.vector(values))
  missing <- is.na(value_text) | value_text %in% c("", "NA", "N/A")
  numeric_values <- suppressWarnings(as.numeric(value_text))
  if (any(!missing & (!grepl(measurement_number_pattern(), value_text) |
                     !is.finite(numeric_values) | numeric_values < 0)))
    stop("Invalid measurements found while selecting peak-picking signals. Review the input cells.")
  numeric_values[missing] <- NA_real_
  totals <- rowSums(matrix(numeric_values, nrow = sum(approved)), na.rm = TRUE)
  if (any(!is.finite(totals))) stop("A peak-picking signal total exceeds the finite numeric range.")
  audit$Signal_total[approved] <- totals
  approved_rows <- which(approved)
  # Highest total wins; the source index provides a stable tie break even when
  # duplicate original identifiers or different suffixes yield the same name.
  ranked <- approved_rows[order(-totals, audit$Source_feature_index[approved_rows])]
  winners <- sort(ranked[!duplicated(audit$Lipid[ranked])])
  audit$Decision[winners] <- "retained"
  kept <- dat[winners, , drop = FALSE]
  kept$Lipid <- audit$Lipid[winners]
  rownames(audit) <- NULL
  presence_filter$counts <- list(input_features = nrow(audit), approved_features = sum(approved),
    excluded_not_approved = sum(!approved), collapsed_duplicates = sum(approved) - length(winners),
    retained_features = length(winners))
  list(data = kept, original_identifiers = audit$Lipid_original[winners],
    audit = audit, presence_filter = presence_filter)
}

# Rebuild all derived review fields after choosing original sample occurrences.
# Pending reviews expose the original tables, but cannot reach processing.
finalize_input_review <- function(inspected, pending = FALSE) {
  dat <- inspected$data
  dat_meta <- inspected$metadata
  settings <- inspected$settings
  sample_ids <- names(dat)[-1L]
  metadata_ids <- as.character(dat_meta[[1L]])
  if (!pending && (anyDuplicated(sample_ids) || anyDuplicated(metadata_ids)))
    stop("Resolve all duplicate sample IDs before finalizing the input review.")

  blank_sample_ids <- sample_ids[grepl(settings$blank_pattern, sample_ids, ignore.case = TRUE)]
  qc_sample_ids <- sample_ids[grepl(settings$qc_pattern, sample_ids, ignore.case = TRUE)]
  if (length(intersect(blank_sample_ids, qc_sample_ids)))
    stop("Blank and QC naming rules overlap. Each sample must have only one role.")
  control_sample_ids <- c(blank_sample_ids, qc_sample_ids)
  study_sample_ids <- setdiff(sample_ids, control_sample_ids)
  if (!length(study_sample_ids))
    stop("No study samples remain after identifying Blank and QC samples. Check the control naming rules, duplicate choices, and input file.")

  unmatched_controls <- setdiff(control_sample_ids, metadata_ids)
  unmatched_study_samples <- setdiff(study_sample_ids, metadata_ids)
  unused_metadata <- setdiff(metadata_ids, sample_ids)
  if (!pending) {
    message("Sample roles: ", length(study_sample_ids), " study, ", length(blank_sample_ids), " Blank, and ", length(qc_sample_ids), " QC.")
    if (length(unmatched_controls)) message("Blank/QC IDs without metadata (allowed): ", length(unmatched_controls), ". Examples: ", paste(utils::head(unmatched_controls, 10L), collapse = ", "))
    message("Study IDs matched to metadata: ", length(study_sample_ids) - length(unmatched_study_samples), " of ", length(study_sample_ids), ".")
    if (length(unused_metadata)) message("Metadata IDs absent from the data (not added): ", length(unused_metadata), ". Examples: ", paste(utils::head(unused_metadata, 10L), collapse = ", "))

    # Validate only retained occurrences. Address columns by position so a raw
    # duplicate header can never redirect validation to its first occurrence.
    numeric_pattern <- measurement_number_pattern()
    for (column in seq.int(2L, ncol(dat))) {
      value_text <- trimws(dat[[column]])
      missing_values <- is.na(value_text) | value_text %in% c("", "NA", "N/A")
      numeric_values <- suppressWarnings(as.numeric(value_text))
      invalid_values <- !missing_values & (!grepl(numeric_pattern, value_text) | !is.finite(numeric_values))
      if (any(invalid_values)) {
        bad_rows <- utils::head(which(invalid_values), 10L)
        examples <- paste(paste0(dat$Lipid[bad_rows], " = ", dat[[column]][bad_rows]), collapse = "; ")
        stop("Invalid measurements in sample ", names(dat)[column], ": ", examples,
          ". Correct these cells or remove unintended annotation rows/columns.")
      }
    }
    message("Validated wide data: ", nrow(dat), analysis_text(" lipid rows and ", settings), length(sample_ids), " sample columns.")
  }

  inspected$sample_ids <- sample_ids
  inspected$study_sample_ids <- study_sample_ids
  inspected$blank_sample_ids <- blank_sample_ids
  inspected$qc_sample_ids <- qc_sample_ids
  inspected$unmatched_study_samples <- unmatched_study_samples
  inspected$unmatched_controls <- unmatched_controls
  inspected$unused_metadata <- unused_metadata
  inspected$sample_roles <- data.frame(Acquired_Sample_ID = sample_ids,
    Role = ifelse(sample_ids %in% blank_sample_ids, "Blank", ifelse(sample_ids %in% qc_sample_ids, "QC", "Study")),
    Metadata_match = sample_ids %in% metadata_ids, stringsAsFactors = FALSE)
  if (!is.null(inspected$sample_batch)) {
    if (length(inspected$sample_batch) != length(sample_ids))
      stop("Batch annotations no longer align with the selected sample occurrences. Review inputs again.")
    inspected$sample_roles$batch <- inspected$sample_batch
  }
  preview_columns <- seq_len(min(8L, ncol(dat)))
  inspected$preview <- dat[seq_len(min(8L, nrow(dat))), preview_columns, drop = FALSE]
  names(inspected$preview) <- names(dat)[preview_columns]
  inspected$summary <- data.frame(
    Metric = analysis_text(c("Input lipid features", "Study samples", "Blank samples", "QC samples", "Matched study samples", "Unmatched study samples", "Unused metadata IDs"), settings),
    Value = c(nrow(dat), length(study_sample_ids), length(blank_sample_ids), length(qc_sample_ids),
      length(setdiff(study_sample_ids, unmatched_study_samples)), length(unmatched_study_samples), length(unused_metadata)))
  inspected
}

# Every duplicated ID requires one keep/exclude decision. A keep decision chooses
# each source independently; a singleton counterpart is retained automatically.
resolve_input_duplicates <- function(inspected, decisions) {
  required <- names(empty_duplicate_resolutions())
  if (!is.data.frame(decisions) || !all(required %in% names(decisions)))
    stop("Duplicate decisions must include sample ID, action, and both occurrence selections.")
  decisions <- decisions[required]
  decisions[] <- lapply(decisions, as.character)
  if (anyNA(decisions) || anyDuplicated(decisions$Acquired_Sample_ID) ||
      !setequal(decisions$Acquired_Sample_ID, inspected$duplicate_ids) ||
      !all(decisions$Action %in% c("keep", "exclude")))
    stop("Choose keep or exclude exactly once for every duplicated sample ID.")
  if (is.null(inspected$duplicate_original) || is.null(inspected$duplicate_occurrences))
    stop("Review the original input files before resolving duplicate sample IDs.")
  decisions <- decisions[match(inspected$duplicate_ids, decisions$Acquired_Sample_ID), , drop = FALSE]
  rownames(decisions) <- NULL
  occurrences <- inspected$duplicate_occurrences
  keep <- rep(FALSE, nrow(occurrences))
  for (row in seq_len(nrow(decisions))) {
    id <- decisions$Acquired_Sample_ID[row]
    for (source in c("Peak-area data", "Metadata")) {
      field <- if (source == "Peak-area data") "Peak_occurrence" else "Metadata_occurrence"
      at <- which(occurrences$Acquired_Sample_ID == id & occurrences$Source == source)
      key <- decisions[[field]][row]
      if (nzchar(key) && !key %in% occurrences$Occurrence_key[at])
        stop("The selected occurrence is stale or does not belong to ", id, " in ", source, ". Review the current inputs again.")
      if (decisions$Action[row] == "exclude") {
        decisions[[field]][row] <- ""
      } else if (length(at) == 1L) {
        decisions[[field]][row] <- occurrences$Occurrence_key[at]
        keep[at] <- TRUE
      } else if (length(at) > 1L) {
        if (!nzchar(key)) stop("Choose one occurrence of ", id, " in ", source, ".")
        keep[at[match(key, occurrences$Occurrence_key[at])]] <- TRUE
      }
    }
  }

  dat <- inspected$duplicate_original$data
  metadata <- inspected$duplicate_original$metadata
  removed_data <- occurrences$Index[!keep & occurrences$Source == "Peak-area data"]
  retained_columns <- setdiff(seq_len(ncol(dat)), removed_data)
  inspected$data <- dat[, retained_columns, drop = FALSE]
  names(inspected$data) <- names(dat)[retained_columns]
  if (!is.null(inspected$duplicate_original$sample_batch))
    inspected$sample_batch <- inspected$duplicate_original$sample_batch[retained_columns[-1L] - 1L]
  removed_metadata <- occurrences$Index[!keep & occurrences$Source == "Metadata"]
  inspected$metadata <- metadata[setdiff(seq_len(nrow(metadata)), removed_metadata), , drop = FALSE]
  rownames(inspected$metadata) <- NULL
  inspected$duplicate_resolutions <- decisions
  inspected$duplicates_resolved <- TRUE
  if (is.null(inspected$duplicate_signal)) inspected$duplicate_signal <- duplicate_signal_summary(inspected)
  audit <- attach_duplicate_signal(occurrences, inspected$duplicate_signal)
  audit$Index <- NULL
  audit$Status <- ifelse(keep, "Retained", "Excluded")
  audit$Decision <- ifelse(decisions$Action[match(audit$Acquired_Sample_ID, decisions$Acquired_Sample_ID)] == "keep", "Keep one", "Exclude ID")
  inspected$duplicate_audit <- if (nrow(audit)) audit else empty_duplicate_audit()
  messages <- inspected$duplicate_import_messages
  inspected <- withCallingHandlers(finalize_input_review(inspected), message = function(m) {
    messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  inspected$messages <- c(messages, paste0("Resolved duplicate sample IDs: ", nrow(decisions), "; excluded IDs: ", sum(decisions$Action == "exclude"), ".\n"))
  inspected
}

inspect_inputs <- function(data_path, metadata_path, settings = default_settings(),
                           metadata_sheet = 1, data_name = basename(data_path),
                           metadata_name = basename(metadata_path),
                           notes_path = NULL, notes_name = NULL) {
  settings <- validate_settings(settings)
  if (settings$processing_mode == "skip") { notes_path <- NULL; notes_name <- NULL }
  notes_enabled <- !is.null(notes_path)
  if (notes_enabled && settings$data_type != "metabolomics")
    stop("Peak-picking notes are supported only for Metabolomics (TCM).")
  if (notes_enabled) {
    if (!is.character(notes_path) || length(notes_path) != 1L || is.na(notes_path))
      stop("The uploaded peak-picking notes file is missing.")
    if (is.null(notes_name)) notes_name <- basename(notes_path)
    if (!is.character(notes_name) || length(notes_name) != 1L || is.na(notes_name) ||
        tolower(tools::file_ext(notes_name)) != "csv")
      stop("Peak-picking notes must be a CSV file.")
  }
  for (path in c(data_path, metadata_path, notes_path)) {
    if (length(path) != 1L || !file.exists(path) || dir.exists(path)) stop("An uploaded input file is missing.")
    if (file.access(path, 4L) != 0L || file.info(path)$size == 0) stop("An input file is unreadable or empty.")
  }
  if (!tolower(tools::file_ext(data_name)) %in% c("csv", "txt", "tsv"))
    stop("Peak-area data must be a CSV, TXT, or TSV wide table.")
  input_file <- data_path
  metadata_file <- metadata_path
  input_delimiter <- settings$input_delimiter
  metadata_delimiter <- settings$metadata_delimiter
  data_orientation <- settings$orientation
  sample_name_column <- settings$sample_name_column
  messages <- character()
  result <- withCallingHandlers({
    # preserve sample IDs, original measurement digits, and missing-value labels
    dat <- read_checked_table(input_file, input_delimiter, preserve_original_headers = notes_enabled || settings$processing_mode == "skip")
    original_headers <- attr(dat, "original_headers")
    attr(dat, "original_headers") <- NULL
    dat_meta <- read_metadata_table(metadata_file, metadata_name, metadata_delimiter, metadata_sheet)

    if (anyNA(names(dat_meta)) || any(names(dat_meta) == "") || anyDuplicated(names(dat_meta)) > 0L) stop("Metadata column names must be nonmissing, nonblank, and unique.")
    if (any(c(".Role", ".Imputed_fraction") %in% names(dat_meta)))
      stop("Metadata columns .Role and .Imputed_fraction are reserved for diagnostics; rename those input columns.")
    metadata_id_column <- if (is.null(settings$metadata_name_column)) 1L else
      which(names(dat_meta) == trimws(settings$metadata_name_column))
    if (length(metadata_id_column) != 1L)
      stop("The selected metadata sample-ID column must match exactly one column in the selected sheet/file.")
    metadata_name_column <- names(dat_meta)[metadata_id_column]
    # Downstream PCA/export code expects the matching key first. Reorder only
    # columns in memory; preserve rows, other fields, and the original workbook.
    dat_meta <- dat_meta[, c(metadata_id_column, setdiff(seq_len(ncol(dat_meta)), metadata_id_column)), drop = FALSE]
    metadata_ids <- trimws(dat_meta[[1L]])
    if (anyNA(metadata_ids) || any(metadata_ids %in% c("", "NA", "N/A")))
      stop("The selected metadata sample-ID column (", metadata_name_column, ") contains missing sample IDs.")
    dat_meta[[1L]] <- metadata_ids

    if (anyNA(names(dat)) || any(names(dat)[-1L] %in% c("", "NA", "N/A"))) stop(analysis_text("Data headers after the first column must contain nonmissing sample or lipid names.", settings))
    header_labels <- normalize_label(names(dat))
    sample_headers <- c("samplename", "sampleid", "acquiredsampleid", "mlasampleid")
    lipid_headers <- c("lipid", "lipids", "compound", "compoundname", "metabolite", "metabolitename")
    if (any(header_labels %in% lipid_headers) && any(header_labels %in% sample_headers) && any(header_labels %in% c("value", "peakarea", "intensity"))) stop("The input appears to be long format. Supply a wide matrix with one identifier column and measurements on the other axis.")


    #### Detect Data Orientation ####

    # use metadata matches and explicit labels, never the number of rows vs columns
    sample_columns <- which(header_labels %in% sample_headers)
    if (!is.null(sample_name_column)) sample_columns <- which(names(dat) == trimws(sample_name_column))
    if (!is.null(sample_name_column) && length(sample_columns) != 1L) stop("The requested sample_name_column must match exactly one input column.")
    if (length(sample_columns) > 1L) stop("Several sample-ID columns were found. Set sample_name_column to the intended column and remove unused annotation columns.")
    sample_id_column <- if (length(sample_columns) == 1L) sample_columns else 1L
    row_ids <- trimws(dat[[sample_id_column]])
    column_ids <- names(dat)[-1L]
    row_matches <- sum(row_ids %in% metadata_ids)
    column_matches <- sum(column_ids %in% metadata_ids)
    sample_row_evidence <- length(sample_columns) == 1L || row_matches > 0L
    lipid_row_evidence <- header_labels[1L] %in% lipid_headers || column_matches > 0L

    if (data_orientation == "auto") {
      if (sample_row_evidence && lipid_row_evidence) stop("Orientation is ambiguous: sample IDs or labels support both axes. Inspect the table, then set data_orientation explicitly if appropriate.")
      if (!sample_row_evidence && !lipid_row_evidence) stop("Cannot determine orientation. Check sample IDs against the selected metadata column, use a recognized identifier header, or set data_orientation explicitly.")
      data_orientation <- if (sample_row_evidence) "samples_in_rows" else "lipids_in_rows"
    }


    #### Standardize to Lipid Rows and Sample Columns ####

    sample_batch <- NULL
    selected_data_column <- if (data_orientation == "samples_in_rows") names(dat)[sample_id_column] else "Column headers"
    if (data_orientation == "samples_in_rows") {
      sample_ids <- trimws(dat[[sample_id_column]])
      sample_positions <- seq_len(nrow(dat))
      sample_axis <- "Imported data row"
      batch_columns <- setdiff(which(header_labels == "batch"), sample_id_column)
      if (length(batch_columns) > 1L) stop("More than one batch annotation column was found.")
      if (length(batch_columns)) sample_batch <- trimws(dat[[batch_columns]])
      annotation_columns <- which(header_labels %in% c("sampletype", "batch"))
      annotation_names <- names(dat)[annotation_columns]
      feature_columns <- setdiff(seq_len(ncol(dat)), c(sample_id_column, annotation_columns))
      if (length(feature_columns) == 0L) stop(analysis_text("No lipid measurement columns remain after removing the sample-ID, Sample Type and batch columns.", settings))
      feature_names <- if (settings$processing_mode == "skip") original_headers[feature_columns] else names(dat)[feature_columns]
      if (notes_enabled) feature_source <- data.frame(
        Source_feature_index = seq_along(feature_columns), Source_axis = "Column",
        Source_position = feature_columns, Lipid_original = original_headers[feature_columns],
        stringsAsFactors = FALSE)
      measurement_matrix <- t(as.matrix(dat[feature_columns]))
      dimnames(measurement_matrix) <- NULL
      dat <- data.frame(Lipid = feature_names, measurement_matrix, check.names = FALSE, stringsAsFactors = FALSE)
      names(dat) <- c("Lipid", sample_ids)
      if (length(annotation_columns)) message("Excluded sample annotation columns from measurements: ", paste(annotation_names, collapse = ", "), ".")
      message(analysis_text("Detected samples in rows; transposed to lipid rows and sample columns.", settings))
    } else {
      sample_positions <- seq.int(2L, ncol(dat))
      sample_axis <- "Column"
      names(dat)[1L] <- "Lipid"
      batch_rows <- which(normalize_label(dat$Lipid) == "batch")
      if (length(batch_rows) > 1L) stop("More than one batch annotation row was found.")
      if (length(batch_rows)) sample_batch <- trimws(as.character(unlist(dat[batch_rows, -1L, drop = FALSE], use.names = FALSE)))
      annotation_rows <- normalize_label(dat$Lipid) %in% c("sampletype", "batch")
      if (notes_enabled) feature_source <- data.frame(
        Source_feature_index = seq_len(sum(!annotation_rows)), Source_axis = "Imported data row",
        Source_position = which(!annotation_rows), Lipid_original = dat$Lipid[!annotation_rows],
        stringsAsFactors = FALSE)
      if (any(annotation_rows)) message("Excluded Sample Type/batch annotation row(s) from measurements: ", sum(annotation_rows), ".")
      dat <- dat[!annotation_rows, , drop = FALSE]
      message(analysis_text("Detected lipids in rows; no transpose needed.", settings))
    }

    if (nrow(dat) == 0L || ncol(dat) < 2L) stop(analysis_text("Expected at least one lipid and one sample after arranging the data.", settings))
    sample_ids <- names(dat)[-1L]
    if (anyNA(sample_ids) || any(sample_ids %in% c("", "NA", "N/A", "Lipid"))) stop("Sample IDs must be nonmissing, nonblank, and different from Lipid.")
    if (anyNA(dat$Lipid) || any(trimws(dat$Lipid) %in% c("", "NA", "N/A"))) stop(analysis_text("The input contains missing lipid names.", settings))

    duplicate_ids <- unique(c(sample_ids[duplicated(sample_ids)], metadata_ids[duplicated(metadata_ids)]))
    occurrences <- empty_duplicate_occurrences()
    if (length(duplicate_ids)) {
      sheet_label <- ""
      if (tolower(tools::file_ext(metadata_name)) %in% c("xlsx", "xls")) {
        sheet_label <- if (is.numeric(metadata_sheet)) readxl::excel_sheets(metadata_path)[metadata_sheet] else as.character(metadata_sheet)
      }
      # Bind occurrence keys to both uploaded contents and the selected layout/sheet.
      # Keys are opaque; the user sees the separate source and position labels.
      review_key <- paste(c(unname(tools::md5sum(c(data_path, metadata_path))), data_orientation,
        sheet_label, selected_data_column, metadata_name_column), collapse = ":")
      peak_at <- which(sample_ids %in% duplicate_ids)
      metadata_at <- which(metadata_ids %in% duplicate_ids)
      if (length(peak_at)) occurrences <- rbind(occurrences, data.frame(
        Occurrence_key = paste(review_key, "data", sample_positions[peak_at], sep = ":"),
        Acquired_Sample_ID = sample_ids[peak_at], Source = "Peak-area data", Filename = data_name,
        Sheet = "", Axis = sample_axis, Position = sample_positions[peak_at], Index = peak_at + 1L,
        stringsAsFactors = FALSE))
      if (length(metadata_at)) occurrences <- rbind(occurrences, data.frame(
        Occurrence_key = paste(review_key, "metadata", metadata_at, sep = ":"),
        Acquired_Sample_ID = metadata_ids[metadata_at], Source = "Metadata", Filename = metadata_name,
        Sheet = sheet_label, Axis = "Imported data row", Position = metadata_at, Index = metadata_at,
        stringsAsFactors = FALSE))
      message("Duplicate sample IDs require review: ", length(duplicate_ids), ".")
    }
    inspected <- list(data = dat, metadata = dat_meta, orientation = data_orientation, settings = settings,
      data_name = data_name, metadata_name = metadata_name, metadata_sheet = metadata_sheet,
      sample_name_column = selected_data_column, metadata_name_column = metadata_name_column,
      sample_batch = sample_batch,
      duplicate_ids = duplicate_ids, duplicates_resolved = !length(duplicate_ids),
      duplicate_occurrences = occurrences, duplicate_original = list(data = dat, metadata = dat_meta, sample_batch = sample_batch),
      duplicate_resolutions = empty_duplicate_resolutions(), duplicate_audit = empty_duplicate_audit())
    inspected$presence_filter <- empty_presence_filter(settings)
    if (notes_enabled) {
      inspected$feature_source <- feature_source
      inspected$presence_filter <- inspect_presence_notes(notes_path, notes_name, feature_source, settings)
    }
    inspected$duplicate_signal <- duplicate_signal_summary(inspected)
    finalize_input_review(inspected, pending = length(duplicate_ids) > 0L)
  }, message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage") })
  result$messages <- messages
  result$duplicate_import_messages <- messages
  result$summary$Metric <- analysis_text(result$summary$Metric, settings)
  result
}

# Add the data-file batch to output metadata only after input matching/review.
# Missing study metadata stays unmatched; a batch value is never a match.
metadata_with_data_batch <- function(metadata, sample_roles) {
  if (!"batch" %in% names(sample_roles)) return(metadata)
  if (anyDuplicated(as.character(metadata[[1L]])) || anyDuplicated(sample_roles$Acquired_Sample_ID))
    stop("Resolve duplicate IDs before attaching batch annotations.")
  # Controls may lack metadata. Add only those control rows, not unmatched studies.
  controls <- sample_roles$Acquired_Sample_ID[sample_roles$Role %in% c("Blank", "QC")]
  extra <- setdiff(controls, as.character(metadata[[1L]]))
  if (length(extra)) {
    added <- metadata[rep(NA_integer_, length(extra)), , drop = FALSE]
    added[[1L]] <- extra
    metadata <- rbind(metadata, added)
  }
  # Preserve any user-provided batch column (and its spelling/values) separately.
  label <- if (any(normalize_label(names(metadata)) == "batch")) "Data batch" else "batch"
  label <- utils::tail(make.unique(c(names(metadata), label), sep = "_"), 1L)
  metadata[[label]] <- sample_roles$batch[match(as.character(metadata[[1L]]), sample_roles$Acquired_Sample_ID)]
  rownames(metadata) <- NULL
  metadata
}

process_data <- function(inspected, exclude_unmatched = FALSE, progress = function(detail) {}) {
  settings <- validate_settings(inspected$settings)
  if (identical(inspected$duplicates_resolved, FALSE) ||
      (length(inspected$duplicate_ids) && !isTRUE(inspected$duplicates_resolved)))
    stop("Resolve all duplicate sample IDs and apply the choices before processing.")
  actual_sample_ids <- names(inspected$data)[-1L]
  actual_metadata_ids <- if (is.null(inspected$metadata)) character() else as.character(inspected$metadata[[1L]])
  if (anyNA(actual_sample_ids) || any(trimws(actual_sample_ids) %in% c("", "NA", "N/A", "Lipid")) ||
      anyDuplicated(trimws(actual_sample_ids)) || anyNA(actual_metadata_ids) ||
      any(trimws(actual_metadata_ids) %in% c("", "NA", "N/A")) || anyDuplicated(trimws(actual_metadata_ids)))
    stop("Processing requires valid, unique sample IDs in both peak-area data and metadata. Review duplicate sample IDs first.")
  if (!identical(actual_sample_ids, as.character(inspected$sample_ids)))
    stop("Sample IDs changed after input review. Review the current inputs again.")
  if (!is.logical(exclude_unmatched) || length(exclude_unmatched) != 1L || is.na(exclude_unmatched))
    stop("Sample exclusion approval must be TRUE or FALSE.")
  if (length(inspected$unmatched_study_samples) && !isTRUE(exclude_unmatched))
    stop("Review unmatched study samples and explicitly approve their exclusion before processing.")
  if (length(setdiff(inspected$study_sample_ids, inspected$unmatched_study_samples)) == 0L)
    stop("No study samples would remain after exclusion. Correct sample IDs or metadata.")
  if (settings$processing_mode == "skip") return(retain_prepared_data(inspected, exclude_unmatched))
  if (!exists("tsn_long", mode = "function")) stop("Load the bundled total_sum_normalization_long.R before processing.")
  dat <- inspected$data
  n_input_features <- nrow(dat)
  presence_filter <- inspected$presence_filter
  if (is.null(presence_filter)) presence_filter <- empty_presence_filter(settings)
  notes_enabled <- identical(presence_filter$status, "applied")
  presence_filter_audit <- empty_presence_filter_audit()
  if (notes_enabled && (settings$data_type != "metabolomics" ||
      !is.data.frame(inspected$feature_source) || nrow(inspected$feature_source) != nrow(dat)))
    stop("Peak-picking feature identities changed after input review. Review the current inputs again.")
  sample_ids <- inspected$sample_ids
  blank_sample_ids <- inspected$blank_sample_ids
  qc_sample_ids <- inspected$qc_sample_ids
  excluded_study_samples <- inspected$unmatched_study_samples
  duplicate_audit <- if (is.null(inspected$duplicate_audit)) empty_duplicate_audit() else inspected$duplicate_audit
  duplicate_resolutions <- if (is.null(inspected$duplicate_resolutions)) empty_duplicate_resolutions() else inspected$duplicate_resolutions
  dat <- dat[, c("Lipid", setdiff(sample_ids, excluded_study_samples)), drop = FALSE]
  sample_ids <- names(dat)[-1L]
  study_sample_ids <- setdiff(sample_ids, c(blank_sample_ids, qc_sample_ids))
  intensity_cutoff <- settings$intensity_threshold
  min_presence <- settings$min_presence
  impute_fraction <- settings$impute_fraction
  save_diagnostic_plots <- TRUE
  plots <- list()
  plot_sizes <- list()
  save_diagnostic_plot <- function(plot, filename, width = 12, height = 9) {
    key <- tools::file_path_sans_ext(filename)
    for (label in names(plot$labels)) if (is.character(plot$labels[[label]]))
      plot$labels[[label]] <- analysis_text(plot$labels[[label]], settings)
    plots[[key]] <<- plot
    plot_sizes[[key]] <<- list(width = width, height = height)
    invisible(plot)
  }
  messages <- inspected$messages
  result <- withCallingHandlers({
    if (length(excluded_study_samples)) message("Excluded unmatched study samples: ", paste(excluded_study_samples, collapse = ", "))
    progress(analysis_text("Cleaning peak areas and lipid names", settings))
    n_negative <- 0L
    for (sample_col in sample_ids) {
      peak_values <- suppressWarnings(as.numeric(dat[[sample_col]]))
      negative_peaks <- is.finite(peak_values) & peak_values < 0
      n_negative <- n_negative + sum(negative_peaks)
      dat[[sample_col]][negative_peaks] <- "0"
    }
    message("Negative peak areas replaced with zero: ", n_negative, ".")


    #### Clean Lipid Names and Missing Values ####

    # input is now validated wide data; the conversion to long format comes afterward

    # remove the d7_SPLASH lipid standards
    # comment out this filter if the standards should stay in the data set
    standards <- dat[stringr::str_detect(dat$Lipid, stringr::fixed("d7_SPLASH")), , drop = FALSE]
    n_standards <- nrow(standards)
    if (notes_enabled) feature_source <- inspected$feature_source[
      !stringr::str_detect(dat$Lipid, stringr::fixed("d7_SPLASH")), , drop = FALSE]
    dat <- dat %>%
      dplyr::filter(!stringr::str_detect(Lipid, stringr::fixed("d7_SPLASH")) | is.na(Lipid))

    if (notes_enabled) {
      selected <- select_presence_signals(dat, feature_source, presence_filter)
      dat <- selected$data
      selected_original_identifiers <- selected$original_identifiers
      presence_filter <- selected$presence_filter
      presence_filter_audit <- selected$audit
      message("Peak-picking notes applied: ", presence_filter$counts$excluded_not_approved,
        " signals excluded; ", presence_filter$counts$collapsed_duplicates,
        " additional approved signals collapsed; ", presence_filter$counts$retained_features,
        " metabolites retained before missingness and detection filtering.")
      all_missing <- Reduce(`&`, lapply(dat[-1L], function(x)
        is.na(x) | trimws(x) %in% c("N/A", "NA", "")))
      selected_original_identifiers <- selected_original_identifiers[!all_missing]
    }

    # identify rows where every sample measurement is missing
    # zero is a measurement and is not removed by this step
    dat_removed_na <- dat %>%
      dplyr::filter(dplyr::if_all(-Lipid, ~ is.na(.x) | trimws(.x) %in% c("N/A", "NA", "")))

    # retain partially missing lipid rows and all of their sample measurements
    # dropping individual missing measurements would change the missingness denominator
    dat <- dat %>%
      dplyr::filter(!dplyr::if_all(-Lipid, ~ is.na(.x) | trimws(.x) %in% c("N/A", "NA", "")))

    if (nrow(dat) == 0L) stop(analysis_text("No lipid rows remain after removing standards and all-missing rows.", settings))

    invalid_retained_names <- if (notes_enabled) "" else c("", "NA", "N/A")
    if (anyNA(dat$Lipid) || any(trimws(dat$Lipid) %in% invalid_retained_names)) {
      stop(analysis_text("Retained rows have missing lipid names. Check the first input column.", settings))
    }

    # keep a copy of the measurements and original names before cleaning
    measurements_before <- dat[-1L]
    lipid_names_original <- if (notes_enabled) selected_original_identifiers else dat$Lipid

    if (!notes_enabled) {
      # keep everything through the last ')' and remove everything after it
      # example: LPC(18:1)+H becomes LPC(18:1)
      # this is the original cleaner's rule: it also removes other trailing annotations
      dat <- dat %>%
        dplyr::mutate(Lipid = gsub("\\)[^)]*$", ")", Lipid)) %>%

        # remove the cholesterol water-loss label when followed by +H
        # Chol-H2O+H and Chol -H20 +H both become Chol
        dplyr::mutate(Lipid = gsub("^(Chol)[[:space:]]*-[[:space:]]*H2[O0][[:space:]]*[+][[:space:]]*H[[:space:]]*$", "\\1", Lipid, ignore.case = TRUE)) %>%

        # remove simple trailing + adducts or -H adducts
        dplyr::mutate(Lipid = gsub("([+][0-9]*[A-Z][A-Za-z0-9]*|-[0-9]*H)$", "", Lipid))

      if (any(trimws(dat$Lipid) == "")) stop(analysis_text("Name cleaning produced an empty lipid name.", settings))

      # make repeated cleaned names unique BEFORE converting to long format
      # example: two LPC(20:3) rows become LPC(20:3)_1 and LPC(20:3)_2
      # retain each source row as a separate feature; do not sum or average adduct signals
      # suffixes follow input row order and skip names already in use after cleaning
      lipid_names <- dat$Lipid
      lipid_names_base <- lipid_names
      duplicate_names <- unique(lipid_names[duplicated(lipid_names)])

      for (lipid_name in duplicate_names) {
        duplicate_rows <- which(lipid_names %in% lipid_name)
        suffix <- 1L

        for (row in duplicate_rows) {
          unique_name <- paste0(lipid_name, "_", suffix)
          while (unique_name %in% dat$Lipid) {
            suffix <- suffix + 1L
            unique_name <- paste0(lipid_name, "_", suffix)
          }
          dat$Lipid[row] <- unique_name
          suffix <- suffix + 1L
        }
      }
    }

    if (anyDuplicated(dat$Lipid) > 0L) stop(analysis_text("Duplicate lipid names remain after cleaning.", settings))

    # confirm that cleaning names did not change any retained measurement strings
    if (!identical(lapply(measurements_before, identity), lapply(dat[-1L], identity))) {
      stop("Measurement values changed during name cleaning.")
    }

    # Keep the original measurement strings in the cleaned export object.
    cleaned <- dat
    if (notes_enabled) lipid_names_base <- dat$Lipid
    lipid_name_key <- tibble::tibble(Lipid_original = lipid_names_original, Lipid = dat$Lipid,
      Lipid_base = lipid_names_base)

    #### Prepare Long Format Data ####

    # the remaining processing steps expect one row per lipid and sample
    # keep missing measurements so every retained lipid has the same sample denominator
    dat <- dat %>%
      tidyr::pivot_longer(cols = -Lipid, names_to = "Acquired_Sample_ID", values_to = "Value", values_drop_na = FALSE) %>%
      dplyr::mutate(Value = trimws(Value), Value = dplyr::if_else(Value %in% c("N/A", "NA", ""), NA_character_, Value))

    # numeric values are required for thresholding, imputation, and normalization
    # the cleaned wide CSV above preserves the original measurement strings
    value_text <- dat$Value
    dat$Value <- suppressWarnings(as.numeric(value_text))

    # stop on unexpected text, nonfinite numbers, or negative peak areas
    # do not silently turn invalid measurements into missing values
    invalid_values <- !is.na(value_text) & (!is.finite(dat$Value) | dat$Value < 0)

    if (any(invalid_values)) {
      stop("Invalid measurements found. Check the displayed cells before processing.")
    }


    observations <- dat

    #### Assess Intensity Threshold ####
    progress("Assessing detection and missingness")

    intensity_prep <- dat %>%
      dplyr::filter(!Acquired_Sample_ID %in% blank_sample_ids) %>%
      dplyr::filter(!Acquired_Sample_ID %in% qc_sample_ids)

    if (nrow(intensity_prep) == 0L) stop("No measurements remain after excluding Blank and QC samples.")

    # assess the threshold among nonmissing measurements only
    # missing measurements are counted separately in the next plot
    n_observed <- sum(!is.na(intensity_prep$Value))
    if (n_observed == 0L) stop("All non-Blank, non-QC measurements are missing.")

    # log spaced thresholds to test
    thresholds <- sort(unique(c(1e3, 5e3, 1e4, 2.5e4, 5e4, 1e5, 2.5e5, 5e5, 1e6, intensity_cutoff)))

    # for each threshold, count nonmissing measurements below that threshold
    intensity_threshold_applied <- tibble::tibble(threshold = thresholds) %>%
      dplyr::mutate(n_removed = purrr::map_int(threshold, ~ sum(intensity_prep$Value < .x, na.rm = TRUE)), pct_removed = n_removed / n_observed * 100)

    # make the plot
    p_intensity_threshold <- ggplot2::ggplot(intensity_threshold_applied, ggplot2::aes(x = threshold, y = pct_removed)) +
      ggplot2::geom_line(color = "dodgerblue", linewidth = 2) +
      ggplot2::geom_point(color = "black", size = 3) +
      ggplot2::geom_vline(xintercept = intensity_cutoff, linetype = "dashed", color = "grey25") +
      ggplot2::scale_x_log10(breaks = 10^(3:6), minor_breaks = rep(1:9, 4) * 10^rep(3:6, each = 9), labels = scales::label_log()) +
      ggplot2::scale_y_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
      ggplot2::annotate("text", x = intensity_cutoff, y = max(intensity_threshold_applied$pct_removed), label = paste0("current (", format(intensity_cutoff, scientific = FALSE, trim = TRUE), ")"), hjust = -0.1, vjust = 1, color = "grey25") +
      ggplot2::labs(x = "Intensity Threshold", y = "Percent of Nonmissing Measurements Below Threshold", title = "Impact of intensity threshold on lipid measurements") +
      ggplot2::theme_minimal()

    save_diagnostic_plot(p_intensity_threshold, "intensity_threshold_sensitivity.pdf", width = 10, height = 7)


    #### Further Test Selected Intensity Threshold ####

    # count present, below-threshold, and missing values separately per sample
    missing_per_sample <- dat %>%
      dplyr::group_by(Acquired_Sample_ID) %>%
      dplyr::summarise(Present = sum(Value >= intensity_cutoff, na.rm = TRUE), Below_threshold = sum(Value < intensity_cutoff, na.rm = TRUE), Missing = sum(is.na(Value)), .groups = "drop") %>%
      tidyr::pivot_longer(cols = c(Present, Below_threshold, Missing), names_to = "Status", values_to = "Count")

    # bar plot of low and missing value counts
    p_detection_per_sample <- ggplot2::ggplot(missing_per_sample, ggplot2::aes(x = Acquired_Sample_ID, y = Count, fill = Status)) +
      ggplot2::geom_col(color = "black", width = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = Count), position = ggplot2::position_stack(vjust = 0.5), size = 3, angle = 90) +
      #scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
      ggplot2::scale_fill_manual(values = c(Present = "dodgerblue", Below_threshold = "grey70", Missing = "grey35")) +
      ggplot2::labs(x = "Sample", y = "Number of values", fill = "") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))

    save_diagnostic_plot(p_detection_per_sample, "per_sample_detection_counts.pdf", width = max(12, min(28, 4 + 0.16 * length(sample_ids))), height = 9)


    #### Missing Value Assessment ####

    # Apply the selected intensity threshold.
    # assumption: NA, N/A, and blank measurements enter the same imputation workflow
    # as nondetections; investigate missingness caused by acquisition or processing failures
    dat_threshold_pre <- dat %>%

      # remove the blanks
      dplyr::filter(!Acquired_Sample_ID %in% blank_sample_ids) %>%

      # remove the QCs
          dplyr::filter(!Acquired_Sample_ID %in% qc_sample_ids) %>%

          # flag QC samples
      dplyr::mutate(is_qc = Acquired_Sample_ID %in% qc_sample_ids) %>%

      # replace missing values and values under the threshold with zero
      dplyr::mutate(Value = dplyr::if_else(is.na(Value) | Value < intensity_cutoff, 0, Value)) %>%

      # safeguard that keeps lipids with >= 1 nonzero value
      # QC samples are excluded from this command
      # Value[!is_qc] restricts the check to non-QC samples only
      dplyr::group_by(Lipid) %>%
      dplyr::filter(any(Value[!is_qc] > 0)) %>%
      dplyr::ungroup()

    if (nrow(dat_threshold_pre) == 0L) stop(analysis_text("No lipids have study-sample values at or above the selected intensity threshold.", settings))

    # visualization
    # calculate the per-lipid missingness from the thresholded data
    # QC samples are not included in the plot or the missingness calculation
    missingness <- dat_threshold_pre %>%
      dplyr::group_by(Lipid) %>%
      dplyr::summarise(pct_missing = mean(Value[!is_qc] == 0) * 100, presence = mean(Value[!is_qc] > 0), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(pct_missing)) %>%
      dplyr::mutate(rank = dplyr::row_number())

    # counts removed when keeping lipids at or below each missingness cutoff
    # The 25%/50% references remain descriptive; the configured presence cutoff controls filtering.
    n_50 <- sum(missingness$pct_missing > 50)
    n_25 <- sum(missingness$pct_missing > 25)

    # ranked curve
    p_lipid_missingness <- ggplot2::ggplot(missingness, ggplot2::aes(x = rank, y = pct_missing)) +
      ggplot2::geom_line(color = "dodgerblue", linewidth = 2) +
      ggplot2::geom_hline(yintercept = c(25, 50), linetype = "dashed", color = "grey25") +
      ggplot2::scale_y_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
      ggplot2::annotate("text", x = max(missingness$rank), y = 50, label = paste0(n_50, " removed"), hjust = 1, vjust = -0.5, color = "grey25") +
      ggplot2::annotate("text", x = max(missingness$rank), y = 25, label = paste0(n_25, " removed"), hjust = 1, vjust = -0.5, color = "grey25") +
      ggplot2::geom_hline(yintercept = 100 * (1 - min_presence), color = "firebrick", linewidth = 0.6) +
      ggplot2::labs(x = "Lipid Rank", y = "Percent Samples Missing", title = "Per-lipid Missingness",
           subtitle = paste0("Selected maximum missingness: ", format(100 * (1 - min_presence)), "% (red line); ",
                             sum(missingness$presence < min_presence), " additional features removed"),
           caption = "Features with no study-sample detections were removed before this plot. Dashed lines show 25% and 50% reference cutoffs.") +
      ggplot2::theme_minimal()

    save_diagnostic_plot(p_lipid_missingness, "per_lipid_missingness.pdf", width = 10, height = 7)

    # apply final filtering
    dat_threshold <- dat_threshold_pre %>%

      # this is an additional missing value filter, adjust this to be project specific
      # QC samples not included in the calculation
      dplyr::group_by(Lipid) %>%
      dplyr::filter(mean(Value[!is_qc] > 0) >= min_presence) %>%
      dplyr::ungroup() %>%

      dplyr::select(-is_qc)

    if (nrow(dat_threshold) == 0L) stop(analysis_text("No lipids pass the final missing-value filter.", settings))


    #### Missing Value Imputation ####
    progress("Imputing and applying total sum normalization")
    # Impute missing/below-threshold values with the selected fraction of each lipid minimum.
    # Blank and QC samples are excluded before this minimum is calculated.
    dat_imputed <- dat_threshold %>%
      dplyr::group_by(Lipid) %>%
      dplyr::mutate(min_nonzero = min(Value[Value > 0], na.rm = TRUE), Value_imputed = dplyr::if_else(Value == 0, impute_fraction * min_nonzero, Value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-min_nonzero) %>%

      # this removes the peak area data which has caused issues with down stream analyses
      # we want to move forward with the imputed data in Value_imputed
      # so we do not need the Value column anymore, best to remove it here
      dplyr::select(-Value)


    #### Total Sum Normalization ####
    # total sum normalize the filtered and imputed data set
    dat_tsn <- dat_imputed %>%

      tsn_long(sample_col = "Acquired_Sample_ID", compound_col = "Lipid", value_col = "Value_imputed") %>%
      dplyr::rename(Value_tsn = Value_imputed)


    #### Prepare Matched Before and After TSN Diagnostics ####
    progress("Preparing matched QC panels")

    # both stages contain the same filtered, imputed measurements
    # this isolates TSN: "Before TSN" is not raw or unimputed data
    # use plotting copies only; do not change the processed data or export columns
    diagnostic_before <- dat_imputed %>%
      dplyr::select(Acquired_Sample_ID, Lipid, Value = Value_imputed)
    diagnostic_after <- dat_tsn %>%
      dplyr::select(Acquired_Sample_ID, Lipid, Value = Value_tsn)
    diagnostic_keys <- c("Acquired_Sample_ID", "Lipid")

    for (diagnostic_stage in list(diagnostic_before, diagnostic_after)) {
      if (anyNA(diagnostic_stage[diagnostic_keys]) || anyDuplicated(diagnostic_stage[diagnostic_keys]) > 0L) stop("Diagnostic sample-feature keys must be nonmissing and unique in each TSN stage.")
      if (!is.numeric(diagnostic_stage$Value) || any(!is.finite(diagnostic_stage$Value) | diagnostic_stage$Value <= 0)) stop("Before/after log2 diagnostics require finite, positive values. Check imputation and the TSN function.")
    }
    if (nrow(dplyr::anti_join(diagnostic_before, diagnostic_after, by = diagnostic_keys)) > 0L || nrow(dplyr::anti_join(diagnostic_after, diagnostic_before, by = diagnostic_keys)) > 0L) stop("TSN changed the sample-feature pairs. Before/after diagnostics require identical pairs.")

    # preserve input sample order, including retained QCs only if explicitly enabled above
    diagnostic_sample_order <- sample_ids[sample_ids %in% diagnostic_before$Acquired_Sample_ID]
    diagnostic_n_features <- dplyr::n_distinct(diagnostic_before$Lipid)
    if (nrow(diagnostic_before) != length(diagnostic_sample_order) * diagnostic_n_features) stop(analysis_text("Before/after diagnostics require the same retained lipid features in every sample.", settings))

    diagnostic_values <- dplyr::bind_rows(
      diagnostic_before %>% dplyr::mutate(Stage = "Before TSN (imputed)"),
      diagnostic_after %>% dplyr::mutate(Stage = "After TSN (imputed)")
    ) %>%
      dplyr::mutate(Stage = factor(Stage, levels = c("Before TSN (imputed)", "After TSN (imputed)")),
             Acquired_Sample_ID = factor(Acquired_Sample_ID, levels = diagnostic_sample_order), Log2_value = log2(Value))

    diagnostic_plot_width <- max(12, min(28, 4 + 0.16 * length(diagnostic_sample_order)))
    diagnostic_axis_size <- if (length(diagnostic_sample_order) > 100L) 4 else 6


    #### Before and After TSN Distribution Panels ####

    # shared sample order and y-axis; no per-feature standardization is applied
    # TSN can shift the entire log2 distribution because its intensity scale changes
    p_tsn_distributions <- ggplot2::ggplot(diagnostic_values, ggplot2::aes(x = Acquired_Sample_ID, y = Log2_value))
    if (diagnostic_n_features >= 2L) {
      p_tsn_distributions <- p_tsn_distributions +
        ggplot2::geom_violin(scale = "width", trim = FALSE, fill = "steelblue", width = 0.7)
    } else {
      message(analysis_text("Only one lipid feature remains: displaying points and boxplots without violin/density estimation.", settings))
      p_tsn_distributions <- p_tsn_distributions + ggplot2::geom_point(color = "steelblue", size = 2)
    }
    p_tsn_distributions <- p_tsn_distributions +
      ggplot2::geom_boxplot(width = 0.12, outlier.size = 0.5, fill = "grey25", alpha = 0.6) +
      ggplot2::facet_wrap(~ Stage, ncol = 1, scales = "fixed") +
      ggplot2::labs(title = "Sample distributions before and after TSN", subtitle = "Identical retained samples and lipid features; both stages include imputed values",
           x = "Sample (input order)", y = "log2 intensity (stage-specific units)") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = diagnostic_axis_size))

    save_diagnostic_plot(p_tsn_distributions, "before_after_TSN_distributions.pdf", width = diagnostic_plot_width, height = 10)


    #### Original Post-TSN Distribution Plot ####

    # also retain the original standalone normalized distribution view
    p_tsn_distribution <- ggplot2::ggplot(dat_tsn, ggplot2::aes(x = Acquired_Sample_ID, y = log2(Value_tsn)))
    if (diagnostic_n_features >= 2L) {
      p_tsn_distribution <- p_tsn_distribution +
        ggplot2::geom_violin(scale = "width", trim = FALSE, fill = "steelblue", width = 0.5)
    } else {
      p_tsn_distribution <- p_tsn_distribution + ggplot2::geom_point(color = "steelblue", size = 2)
    }
    p_tsn_distribution <- p_tsn_distribution +
      ggplot2::geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "grey25", alpha = 0.6) +
      ggplot2::labs(title = "Total Sum Normalized", subtitle = "Filtered and imputed measurements", x = "Sample", y = "log2 TSN intensity") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 5))

    save_diagnostic_plot(p_tsn_distribution, "TSN_sample_distributions.pdf", width = diagnostic_plot_width, height = 8)


    #### Before and After TSN RLE Panels ####

    # reference each lipid to its median across retained samples WITHIN each stage
    # common y-axis allows comparison of RLE centers and spreads
    diagnostic_rle <- diagnostic_values %>%
      dplyr::group_by(Stage, Lipid) %>%
      dplyr::mutate(rle = Log2_value - stats::median(Log2_value)) %>%
      dplyr::ungroup()

    # retain the original post-TSN RLE object for compatibility
    dat_rle <- dat_tsn %>%
      dplyr::mutate(log_val = log2(Value_tsn)) %>%
      dplyr::group_by(Lipid) %>%
      dplyr::mutate(rle = log_val - stats::median(log_val, na.rm = TRUE)) %>%
      dplyr::ungroup()

    p_tsn_rle <- ggplot2::ggplot(diagnostic_rle, ggplot2::aes(x = Acquired_Sample_ID, y = rle)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.3, fill = "steelblue", alpha = 0.5, linewidth = 0.3) +
      ggplot2::facet_wrap(~ Stage, ncol = 1, scales = "fixed") +
      ggplot2::labs(title = "Relative Log Expression (RLE) before and after TSN",
           subtitle = "Each lipid is centered on its stage-specific median across samples",
           x = "Sample (input order)", y = "Relative log2 intensity",
           caption = "RLE is descriptive. Treating offsets as technical assumes most lipid features are biologically stable.") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = diagnostic_axis_size))

    save_diagnostic_plot(p_tsn_rle, "before_after_TSN_RLE.pdf", width = diagnostic_plot_width, height = 10)


    #### Original Post-TSN RLE Plot ####

    # also retain the original standalone normalized RLE view
    p_tsn_rle_original <- ggplot2::ggplot(dat_rle, ggplot2::aes(x = Acquired_Sample_ID, y = rle)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.3, fill = "steelblue", alpha = 0.5, linewidth = 0.3) +
      ggplot2::labs(title = "Relative Log Expression (RLE)", subtitle = "Total Sum Normalized", x = "Sample", y = "Relative log2 intensity") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 5))

    save_diagnostic_plot(p_tsn_rle_original, "TSN_RLE.pdf", width = diagnostic_plot_width, height = 8)


    #### Before and After TSN Density Panels ####

    # one curve per sample, across the same retained lipid features
    # choose one bandwidth in log2 units from the median within-sample bandwidth
    # BEFORE TSN; pooling shifted stages would artificially enlarge the bandwidth
    # use that exact numerical bandwidth for every sample and both stages
    p_tsn_density <- NULL
    diagnostic_density_curves <- NULL
    diagnostic_density_mean <- NULL
    density_bandwidth <- NULL
    density_limits <- NULL
    if (diagnostic_n_features >= 2L) {
      density_bandwidths <- diagnostic_values %>%
        dplyr::filter(Stage == "Before TSN (imputed)") %>%
        dplyr::group_by(Acquired_Sample_ID) %>%
        dplyr::summarise(bandwidth = if (dplyr::n_distinct(Log2_value) > 1L) stats::bw.nrd0(Log2_value) else NA_real_, .groups = "drop")
      valid_bandwidths <- density_bandwidths$bandwidth[is.finite(density_bandwidths$bandwidth) & density_bandwidths$bandwidth > 0]
      density_bandwidth <- if (length(valid_bandwidths) > 0L) stats::median(valid_bandwidths) else 0.25
      if (length(valid_bandwidths) == 0L) message("All pre-TSN samples have constant log2 intensities; using a display-only density bandwidth of 0.25 log2 units.")

      # The identical x-grid permits pointwise averaging within each stage.
      # Three bandwidths beyond the observed range display the smoothed tails.
      density_limits <- range(diagnostic_values$Log2_value) + c(-3, 3) * density_bandwidth
      diagnostic_density_curves <- diagnostic_values %>%
        dplyr::group_by(Stage, Acquired_Sample_ID) %>%
        dplyr::group_modify(~ {
          sample_density <- stats::density(.x$Log2_value, bw = density_bandwidth,
                                           from = density_limits[1], to = density_limits[2], n = 2048)
          tibble::tibble(Log2_value = sample_density$x, Density = sample_density$y)
        }) %>%
        dplyr::ungroup()

      # Every sample has equal weight. This is a descriptive distribution mean,
      # not the density of lipid intensities averaged across samples.
      diagnostic_density_mean <- diagnostic_density_curves %>%
        dplyr::group_by(Stage, Log2_value) %>%
        dplyr::summarise(Density = mean(Density), .groups = "drop")

      p_tsn_density <- ggplot2::ggplot(diagnostic_density_curves, ggplot2::aes(x = Log2_value, y = Density, group = Acquired_Sample_ID, color = Acquired_Sample_ID)) +
        ggplot2::geom_line(linewidth = settings$density_sample_linewidth, alpha = settings$density_sample_alpha) +
        # Draw the summary last so the bold black line remains in front.
        ggplot2::geom_line(data = diagnostic_density_mean, ggplot2::aes(x = Log2_value, y = Density, group = Stage),
                  inherit.aes = FALSE, color = "black", linewidth = settings$density_mean_linewidth, alpha = 1) +
        ggplot2::facet_wrap(~ Stage, ncol = 1, scales = "fixed") +
        ggplot2::labs(title = "Sample density curves before and after TSN",
             subtitle = "Faint curves: individual samples | Bold black curve: mean sample density",
             x = "log2 intensity", y = "Density", color = "Sample", 
             caption = "Curves summarize distributions, not total signal. Both stages include imputed values; low-intensity structure can reflect imputation.") +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = if (length(diagnostic_sample_order) <= 15L) "bottom" else "none") +
        ggplot2::guides(color = ggplot2::guide_legend(ncol = 3))

      if (length(diagnostic_sample_order) > 15L) message("Density legend hidden for readability (>15 samples); colors are consistent between panels.")
      save_diagnostic_plot(p_tsn_density, "before_after_TSN_density.pdf", width = 12, height = 10)
    } else if (save_diagnostic_plots) {
      # replace any older density PDF so a rerun cannot leave a misleading old result
      p_tsn_density <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = analysis_text("Density estimation skipped: only one lipid feature remains.", settings), size = 4) +
        ggplot2::labs(title = "Sample density curves before and after TSN") + ggplot2::theme_void()
      save_diagnostic_plot(p_tsn_density, "before_after_TSN_density.pdf", width = 12, height = 7)
    }



    normalized_wide <- dat_tsn %>%
      tidyr::pivot_wider(id_cols = Lipid, names_from = Acquired_Sample_ID, values_from = Value_tsn)
    n_final_features <- dplyr::n_distinct(dat_tsn$Lipid)
    n_no_detections <- nrow(cleaned) - dplyr::n_distinct(dat_threshold_pre$Lipid)
    n_presence_removed <- dplyr::n_distinct(dat_threshold_pre$Lipid) - n_final_features
    n_imputed <- sum(dat_threshold$Value == 0)
    # The helper normalizes each sample total to the mean of pre-TSN totals.
    normalization_factors <- dat_imputed %>%
      dplyr::group_by(Acquired_Sample_ID) %>% dplyr::summarise(Total_before_TSN = sum(Value_imputed), .groups = "drop") %>%
      dplyr::mutate(Target_total = mean(Total_before_TSN), Scale_factor = Target_total / Total_before_TSN)
    summary <- data.frame(
      Metric = c("Input lipid features", "Negative peak areas set to zero", "Standards preserved separately", "All-missing lipid rows removed",
                 "Cleaned lipid features", "Unmatched study samples excluded", "Retained study samples", "Blank controls excluded before imputation",
                 "QC controls retained separately for comparison", "Lipids with no study detections removed", "Lipids removed by detection-proportion filter",
                 "Retained lipid features", "Imputed measurements", "Final measurements",
                 "Duplicate sample IDs reviewed", "Duplicate IDs excluded entirely", "Duplicate-review peak-area occurrences excluded", "Duplicate-review metadata rows excluded"),
      Value = c(n_input_features, n_negative, n_standards, nrow(dat_removed_na), nrow(cleaned),
                length(excluded_study_samples), length(study_sample_ids), length(blank_sample_ids), length(qc_sample_ids),
                n_no_detections, n_presence_removed, n_final_features, n_imputed, nrow(dat_tsn),
                nrow(duplicate_resolutions), sum(duplicate_resolutions$Action == "exclude"),
                sum(duplicate_audit$Source == "Peak-area data" & duplicate_audit$Status == "Excluded"),
                sum(duplicate_audit$Source == "Metadata" & duplicate_audit$Status == "Excluded")))
    if (notes_enabled) summary <- rbind(summary, data.frame(
      Metric = c("Signals excluded by peak-picking notes", "Additional approved signals collapsed",
        "Metabolites retained by peak-picking notes before detection filtering"),
      Value = c(presence_filter$counts$excluded_not_approved,
        presence_filter$counts$collapsed_duplicates, presence_filter$counts$retained_features)))
    progress("Processing complete")
    list(cleaned = cleaned, standards = standards, observations = observations,
         removed_na = dat_removed_na, lipid_name_key = lipid_name_key,
         imputed = dat_imputed, normalized_long = dat_tsn, normalized_wide = normalized_wide,
         plots = plots, plot_sizes = plot_sizes, summary = summary, settings = settings,
         excluded_samples = excluded_study_samples, normalization_factors = normalization_factors,
         duplicate_audit = duplicate_audit, duplicate_resolutions = duplicate_resolutions,
         presence_filter = presence_filter, presence_filter_audit = presence_filter_audit,
         duplicate_signal = if (is.null(inspected$duplicate_signal)) empty_duplicate_signal() else inspected$duplicate_signal,
         missingness = missingness, detection_counts = missing_per_sample,
         density_curves = diagnostic_density_curves, density_mean = diagnostic_density_mean,
         density_bandwidth = density_bandwidth, density_limits = density_limits,
         data_name = inspected$data_name, metadata_name = inspected$metadata_name,
         metadata = metadata_with_data_batch(inspected$metadata, inspected$sample_roles),
         sample_roles = inspected$sample_roles, orientation = inspected$orientation,
         unused_metadata = inspected$unused_metadata)
  }, message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage") })
  result$messages <- messages
  result$summary$Metric <- analysis_text(result$summary$Metric, settings)
  result
}

active_processing_settings <- function(result) {
  settings <- result$settings
  if (identical(settings$processing_mode, "skip")) settings <- settings[setdiff(names(settings),
    c("intensity_threshold", "min_presence", "impute_fraction", "density_sample_alpha", "density_sample_linewidth", "density_mean_linewidth"))]
  settings
}

