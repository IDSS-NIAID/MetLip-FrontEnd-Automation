# Internal processing module; all mutable state is created per session.
matching_column_names <- function(upload, delimiter = "auto", sheet = 1) {
  if (is.null(upload)) return(list(columns = character(), error = NULL))
  tryCatch({
    headers <- names(suppressMessages(read_metadata_table(upload$datapath,
      upload$name, delimiter = delimiter, sheet = sheet)))
    # Empty headers cannot identify a matching key. Other import validation is
    # deliberately left to the existing pipeline and input-review workflow.
    list(columns = unique(headers[!is.na(headers) & nzchar(trimws(headers))]), error = NULL)
  }, error = function(e) list(columns = character(), error = conditionMessage(e)))
}

metlip_processing_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::attachDependencies(shiny::div(
    id = ns("container"), class = "metlip-processing",
    `data-metlip-prefix` = ns(""),
  shiny::div(class = "app-header", shiny::h1(shiny::textOutput(ns("app_title"), inline = TRUE)),
    shiny::p("Review inputs, compare normalization and scaling, then commit and export a documented workflow."),
    shiny::span(class = "app-version", "v2.4.7")),
  shiny::uiOutput(ns("report_setup_status")),
  htmltools::tagAppendAttributes(shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::h4("1. Select inputs"),
      shiny::radioButtons(ns("data_type"), "Data type", choices = c("Metabolomics (TCM)" = "metabolomics", "Lipidomics (TBL)" = "lipidomics"), selected = "lipidomics"),
      shiny::textInput(ns("project_title"), "Project / report title", analysis_terms()$default_title),
      shiny::textInput(ns("author"), "Author", ""),
      shiny::textInput(ns("metlip_id"), "MetLip ID", ""),
      shiny::textInput(ns("tas_id"), "TAS ID", ""),
      shiny::helpText("Optional details shown at the top of the HTML report. MetLip ID also prefixes download filenames."),
      shiny::fileInput(ns("peak_file"), "Peak-area data", accept = c(".csv", ".txt", ".tsv")),
      shiny::conditionalPanel("input.orientation !== 'lipids_in_rows'", ns = ns,
        shiny::selectInput(ns("sample_column"), "Data sample-ID column",
          choices = c("Auto-detect" = ""), selected = "")),
      shiny::fileInput(ns("meta_file"), "Sample metadata", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
      shiny::conditionalPanel("output.is_excel", ns = ns, shiny::selectInput(ns("meta_sheet"), "Metadata sheet", choices = character(0))),
      shiny::selectInput(ns("metadata_column"), "Metadata sample-ID column", choices = character(0)),
      shiny::uiOutput(ns("matching_column_status")),
      shiny::helpText("Choose the columns containing the same sample IDs. The metadata ID column can be anywhere. For lipids/metabolites in rows, data sample IDs come from column headers. Other metadata columns become PCA color and shape choices. Each upload may be up to 100 MB."),
      shiny::helpText("Sample Type and batch annotations in the data file are excluded from lipid/metabolite measurements. Batch labels are preserved as text and available for PCA coloring (Data batch if metadata already has a batch column)."),
      shiny::conditionalPanel("input.data_type === 'metabolomics' && input.processing_mode !== 'skip'", ns = ns,
        shiny::uiOutput(ns("notes_upload")),
        shiny::actionButton(ns("clear_notes"), "Clear notes"),
        shiny::helpText("Optional CSV with Metabolite and Present columns. Only exact Y entries approve original identifiers. Uploading notes also selects one signal per metabolite before intensity filtering.")),
      shiny::h4("2. Choose processing"),
      shiny::selectInput(ns("processing_mode"), "Processing", c("Process peak-area data" = "process",
        "Skip processing — use prepared data" = "skip"), selected = "process"),
      shiny::conditionalPanel("input.processing_mode === 'skip'", ns = ns,
        shiny::helpText("Retain supplied measurements, including zeros and negatives. No filtering, imputation, feature-name cleaning or standard removal. Existing sample-role and matching rules still apply."),
        shiny::h4("Optional PCA adjustments"),
        shiny::selectInput(ns("direct_method"), "Normalization", stats::setNames(names(normalization_labels), normalization_labels), selected = "none"),
        shiny::selectInput(ns("direct_transform"), "Transformation", c("None" = "none", "Log2" = "log2"), selected = "none"),
        shiny::selectInput(ns("direct_scale"), "Feature scaling", stats::setNames(names(scaling_labels), scaling_labels), selected = "as_is"),
        shiny::helpText("All adjustments default to None. Changes update PCA and downloads immediately. Missing study values must be resolved upstream; no imputation is added. TSN/median/PQN and log2 require positive values.")),
      shiny::conditionalPanel("input.processing_mode !== 'skip'", ns = ns,
      shiny::numericInput(ns("threshold"), "Intensity threshold (inclusive)", 10000, min = 1, step = 1000),
      shiny::numericInput(ns("presence"), "Minimum study-sample detection (%)", 50, min = 0, max = 100, step = 5),
      shiny::numericInput(ns("impute"), "Imputation: % of lipid minimum", 20, min = 0.01, max = 100, step = 5),
      shiny::tags$details(shiny::tags$summary("Density plot appearance"),
        shiny::sliderInput(ns("density_alpha"), "Individual sample opacity", min = 0, max = 1, value = 0.15, step = 0.05),
        shiny::numericInput(ns("density_sample_width"), "Individual curve line width", value = 0.5, min = 0.05, step = 0.1),
        shiny::numericInput(ns("density_mean_width"), "Mean curve line width", value = 1.3, min = 0.05, step = 0.1),
        shiny::helpText("The bold black curve is the equally weighted mean of the sample density curves within each stage."))),
      shiny::tags$details(shiny::tags$summary("Import and sample-role settings"),
        shiny::selectInput(ns("orientation"), "Data orientation", choices = c("Auto-detect" = "auto", "Lipids in rows" = "lipids_in_rows", "Samples in rows" = "samples_in_rows")),
        shiny::selectInput(ns("data_delim"), "Data delimiter", choices = c("Auto-detect" = "auto", "Comma" = ",", "Tab" = "\t")),
        shiny::selectInput(ns("meta_delim"), "Metadata delimiter (text files)", choices = c("Auto-detect" = "auto", "Comma" = ",", "Tab" = "\t")),
        shiny::textInput(ns("blank_pattern"), "Blank sample-ID pattern", "^Blank(_[0-9]+)?(_batch_[0-9]+)?$"),
        shiny::textInput(ns("qc_pattern"), "QC sample-ID pattern", "^QC(_[0-9]+)?(_batch_[0-9]+)?$"),
        shiny::helpText("Defaults recognize Blank/QC names with optional numbers and batch suffixes, such as Blank_0_batch_0001 and QC_1_batch_0002. Patterns are case-insensitive. Sample-ID matching to metadata is case-sensitive.")),
      shiny::hr(), shiny::actionButton(ns("inspect"), "Review inputs", class = "btn-primary", width = "100%"),
      shiny::br(), shiny::br(), shiny::actionButton(ns("run"), "Run processing", class = "btn-primary", width = "100%"),
      shiny::helpText("Changing an input or processing setting clears the prior review and results.")),
    shiny::mainPanel(width = 9,
      shiny::uiOutput(ns("status")),
      shiny::tabsetPanel(id = ns("tabs"),
        shiny::tabPanel("Input review", value = "review",
          shiny::uiOutput(ns("review_panel"))),
        shiny::tabPanel("QC plots", value = "plots",
          shiny::uiOutput(ns("qc_plot_status")),
          shiny::selectInput(ns("plot_name"), "Diagnostic plot", choices = character(0), width = "100%"),
          shiny::uiOutput(ns("plot_panel")), shiny::uiOutput(ns("plot_download"))),
        shiny::tabPanel("Compare preprocessing", value = "compare", comparison_ui(ns("compare"), processing_ns = ns)),
        shiny::tabPanel("PCA", value = "pca", pca_ui(ns("pca"))),
        shiny::tabPanel("Results & downloads", value = "results",
          shiny::uiOutput(ns("results_panel"))),
        shiny::tabPanel("Methods & help", value = "help",
          shiny::h4("Prepared data"),
          shiny::p("Choose Skip processing, review inputs, then Load prepared data to open PCA directly. All optional adjustments default to None. Supplied feature identifiers and measurements are retained, including standards, negative values and zeros. Blanks remain excluded from PCA; incomplete QCs are listed as ineligible. Missing study values prevent PCA without removing data."),
          shiny::p("None — no centering or scaling fits the supplied matrix as selected. Component percentages then describe uncentered sums of squares. Center only subtracts feature means; Pareto and unit scaling also rescale features."),
          shiny::h4("Processing order (when processing is enabled)"),
          shiny::tags$ol(shiny::tags$li("Validate imports, determine orientation and match study sample IDs to metadata."),
            shiny::tags$li("Resolve duplicate sample IDs by selecting one occurrence per file or excluding the ID from both files. Then review unmatched study IDs before excluding any samples."),
            shiny::tags$li("Replace negative peaks with zero and preserve d7_SPLASH standards separately. For TCM with peak-picking notes, approve original identifiers, remove one trailing _1, _2, _3, _pos, or _1pos, and keep the signal with the largest total across remaining study, QC, and blank samples. Otherwise use the existing feature-label cleaning."),
            shiny::tags$li("Apply the intensity threshold. Notes approval is separate from the minimum study-sample detection setting."),
            shiny::tags$li("Filter study features by detection proportion; impute using the selected fraction of each feature minimum."),
            shiny::tags$li("Compare none, TSN, median and PQN using identical features and study imputation. PQN uses the study-sample median reference and does not require QCs. QCs use the same study-derived imputation values for factors and projection; observed QC measurements alone support RSD."),
            shiny::tags$li("Choose log2 or no transformation, then no centering/scaling, center-only, Pareto or z-scaling. Inspect scores, loadings, QC and distribution diagnostics."),
            shiny::tags$li("Commit the workflow and rationale; export normalized intensities and the exact analysis matrix separately.")),
          shiny::h4("Assumptions and limitations"),
          shiny::p("Original missing values and measurements below the intensity threshold share a small-value imputation rule. Investigate their cause and review imputation sensitivity. Samples and features are fixed across candidate methods."),
          shiny::p("QC/Blank roles use the import naming rules. QCs retained after input review are evaluated together as one assumed homogeneous pool across batches, with no additional manual exclusions during comparison. Fixed QC criteria require 80% detection per sample and feature, at least 3 observed measurements per feature, and use a 30% RSD threshold. Preserved standards are available as tables and CSV exports; they are not used for internal-standard calibration."),
          shiny::p("No automated best method or hypothesis test is provided. Distribution alignment and PCA separation can be consequences of preprocessing. None is a meaningful baseline when biology or upstream quantitative normalization should be preserved."),
          shiny::p("Metadata can set point color and shape. Batch correction, injection-order and replicate analyses are not part of this workflow. Feature plots use input sample index, not acquisition order. The comparison tab fits study samples and projects QCs. In final PCA, choose study-only, study fit with QC projections, or a combined study + QC fit."),
          shiny::h4("Exports and reporting"),
          shiny::p("The ZIP contains all comparison factors, masks, settings and selected data. The analysis matrix contains exactly the selected adjustments. Check its settings before applying any further transformation or scaling."),
          shiny::p("Results & downloads lets you browse complete tables and save each as CSV or TSV. Download all tables as a CSV ZIP, or choose the full bundle for tables, PDF plots and models. The HTML Quarto report summarizes the committed run; its editable source is also available."),
          shiny::p("HTML rendering requires the Quarto CLI and report packages. Table, PDF, RDS and report-source exports remain available without the CLI."),
          shiny::h4("Validation"),
          shiny::p("Processing preserves the validated MetLip v2.4.7 workflow."))))), class = "app-layout")

), list(metlip_processing_dependency(), metlip_processing_module_dependency()))
}

metlip_processing_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reviewed <- shiny::reactiveVal(NULL)
    reviewed_signature <- shiny::reactiveVal(NULL)
    approved_signature <- shiny::reactiveVal(NULL)
    duplicate_source <- shiny::reactiveVal(NULL)
    duplicate_choices <- shiny::reactiveVal(NULL)
    applied_duplicate_choices <- shiny::reactiveVal(NULL)
    duplicate_selection_report <- shiny::reactiveVal(NULL)
    review_revision <- shiny::reactiveVal(0L)
    review_epoch <- shiny::reactiveVal(0L)
    notes_upload_generation <- shiny::reactiveVal(0L)
    result <- shiny::reactiveVal(NULL)
    notice <- shiny::reactiveVal(list(kind = "info", text = "Choose both files, then click Review inputs."))
    terms <- shiny::reactive(analysis_terms(input$data_type %||% "lipidomics"))
    notes_input_id <- shiny::reactive(paste0("notes_file_", notes_upload_generation()))
    notes_upload <- shiny::reactive({
      if (terms()$id != "metabolomics" || identical(input$processing_mode, "skip")) return(NULL)
      input[[notes_input_id()]]
    })
    output$notes_upload <- shiny::renderUI({
      if (terms()$id != "metabolomics") return(NULL)
      shiny::fileInput(ns(notes_input_id()), "Peak picking notes (.csv, optional)", accept = ".csv")
    })
    clear_notes_upload <- function() {
      # A fresh input ID also ignores uploads that finish after Clear or a mode change.
      notes_upload_generation(shiny::isolate(notes_upload_generation()) + 1L)
    }
    shiny::observeEvent(input$clear_notes, clear_notes_upload(), priority = 200)
    automatic_titles <- c(analysis_terms("lipidomics")$default_title, analysis_terms("metabolomics")$default_title,
      "Lipidomics processing & QC", "Metabolomics processing & QC")
    project_title <- shiny::reactive({
      title <- trimws(input$project_title %||% "")
      if (!nzchar(title) || title %in% automatic_titles) terms()$default_title else title
    })
    report_details <- shiny::reactive(list(author = trimws(input$author %||% ""),
      metlip_id = trimws(input$metlip_id %||% ""), tas_id = trimws(input$tas_id %||% "")))
    output$app_title <- shiny::renderText(terms()$default_title)
    shiny::observeEvent(input$data_type, {
      t <- terms()
      if (t$id == "lipidomics") clear_notes_upload()
      title <- trimws(input$project_title %||% "")
      if (!nzchar(title) || title %in% automatic_titles)
        shiny::updateTextInput(session, "project_title", value = t$default_title)
      shiny::updateNumericInput(session, "impute", label = paste("Imputation: % of", t$feature, "minimum"))
      shiny::updateSelectInput(session, "compare-feature", label = paste("Inspect", t$feature))
      shiny::updateSelectInput(session, "orientation", choices = stats::setNames(
        c("auto", "lipids_in_rows", "samples_in_rows"),
        c("Auto-detect", paste(if (t$id == "metabolomics") "Metabolites" else "Lipids", "in rows"), "Samples in rows")),
        selected = input$orientation %||% "auto")
    }, ignoreNULL = FALSE, priority = 150)

    output$is_excel <- shiny::reactive({
      !is.null(input$meta_file) && tolower(tools::file_ext(input$meta_file$name)) %in% c("xlsx", "xls")
    })
    shiny::outputOptions(output, "is_excel", suspendWhenHidden = FALSE)
    shiny::observeEvent(input$meta_file, {
      if (tolower(tools::file_ext(input$meta_file$name)) %in% c("xlsx", "xls")) {
        sheets <- tryCatch(readxl::excel_sheets(input$meta_file$datapath), error = function(e) character(0))
        shiny::updateSelectInput(session, "meta_sheet", choices = sheets, selected = if (length(sheets)) sheets[1] else character(0))
      } else shiny::updateSelectInput(session, "meta_sheet", choices = character(0))
    }, ignoreNULL = TRUE, priority = 200)

    selected_metadata_sheet <- shiny::reactive({
      sheet <- input$meta_sheet
      if (is.null(sheet) || !nzchar(sheet)) 1 else sheet
    })
    peak_columns <- shiny::reactive(matching_column_names(input$peak_file, input$data_delim))
    metadata_columns <- shiny::reactive(matching_column_names(input$meta_file, input$meta_delim,
      selected_metadata_sheet()))
    shiny::observeEvent(peak_columns(), {
      columns <- peak_columns()$columns
      selected <- shiny::isolate(input$sample_column)
      if (length(selected) != 1L || !selected %in% columns) selected <- ""
      shiny::updateSelectInput(session, "sample_column",
        choices = c("Auto-detect" = "", stats::setNames(columns, columns)), selected = selected)
    }, ignoreNULL = FALSE, priority = 175)
    shiny::observeEvent(metadata_columns(), {
      columns <- metadata_columns()$columns
      selected <- shiny::isolate(input$metadata_column)
      if (length(selected) != 1L || !selected %in% columns)
        selected <- if (length(columns)) columns[1L] else character()
      shiny::updateSelectInput(session, "metadata_column", choices = stats::setNames(columns, columns),
        selected = selected)
    }, ignoreNULL = FALSE, priority = 175)
    output$matching_column_status <- shiny::renderUI({
      errors <- c(peak_columns()$error, metadata_columns()$error)
      if (length(errors)) shiny::div(class = "notice error", paste(errors, collapse = "\n"))
    })

    shiny::observeEvent(input$processing_mode, {
      skip <- identical(input$processing_mode, "skip")
      shiny::updateActionButton(session, "run", label = if (skip) "Load prepared data" else "Run processing")
      if (skip) {
        shiny::updateSelectInput(session, "direct_method", selected = "none")
        shiny::updateSelectInput(session, "direct_transform", selected = "none")
        shiny::updateSelectInput(session, "direct_scale", selected = "as_is")
      }
    }, ignoreNULL = FALSE, priority = 150)

    settings <- shiny::reactive({
      s <- default_settings()
      s$data_type <- terms()$id
      s$processing_mode <- input$processing_mode %||% "process"
      if (s$processing_mode == "process") {
      s["intensity_threshold"] <- list(input$threshold)
      s$min_presence <- input$presence / 100
      s$impute_fraction <- input$impute / 100
      }
      s$input_delimiter <- input$data_delim
      s$metadata_delimiter <- input$meta_delim
      s$orientation <- input$orientation
      s$sample_name_column <- if (identical(input$orientation, "lipids_in_rows") ||
        is.null(input$sample_column) || !nzchar(input$sample_column)) NULL else input$sample_column
      s$metadata_name_column <- if (is.null(input$metadata_column) || !nzchar(input$metadata_column)) NULL else input$metadata_column
      s$blank_pattern <- input$blank_pattern
      s$qc_pattern <- input$qc_pattern
      if (s$processing_mode == "process") {
      s["density_sample_alpha"] <- list(input$density_alpha)
      s["density_sample_linewidth"] <- list(input$density_sample_width)
      s["density_mean_linewidth"] <- list(input$density_mean_width)
      }
      s
    })
    signature <- shiny::reactive(list(data = input$peak_file, metadata = input$meta_file,
      notes = notes_upload(), notes_generation = notes_upload_generation(),
      sheet = input$meta_sheet, metadata_column = input$metadata_column,
      title = project_title(), report_details = report_details(), settings = settings()))
    reset_duplicate_review <- function() {
      duplicate_source(NULL); duplicate_choices(NULL); applied_duplicate_choices(NULL)
      duplicate_selection_report(NULL)
      review_epoch(shiny::isolate(review_epoch()) + 1L)
      review_revision(shiny::isolate(review_revision()) + 1L)
    }
    shiny::observeEvent(signature(), {
      reviewed(NULL); reviewed_signature(NULL); approved_signature(NULL); result(NULL)
      reset_duplicate_review()
      notice(list(kind = "info", text = "Inputs or settings changed. Click Review inputs before running."))
    }, ignoreInit = TRUE, priority = 100)

    review_ready <- shiny::reactive({
      # Applying decisions updates reviewed(), but must not rebuild the parent UI
      # and reset the browser's duplicate editor controls.
      !is.null(reviewed_signature()) && identical(reviewed_signature(), signature())
    })
    current_review <- shiny::reactive({
      # The status banner gives instructions once; empty outputs stay silent.
      shiny::req(review_ready(), !is.null(reviewed()))
      reviewed()
    })
    approval_key <- shiny::reactive(list(signature = signature(), revision = review_revision()))
    duplicates_applied <- shiny::reactive({
      x <- current_review()
      !length(x$duplicate_ids) || (isTRUE(x$duplicates_resolved) &&
        identical(duplicate_choices(), applied_duplicate_choices()))
    })
    base_result <- shiny::reactive({
      r <- result()
      shiny::validate(shiny::need(!is.null(r) && identical(r$signature, signature()) &&
        identical(r$review_revision, review_revision()),
        "Review inputs, approve any exclusions, then run processing or load prepared data to see results."))
      r
    })
    comparison <- comparison_server("compare", base_result)
    selected_workflow <- shiny::reactive({
      r <- base_result()
      if (is_prepared_result(r)) {
        method <- input$direct_method %||% "none"
        shiny::validate(normalization_method_error(method))
        prepared_selection(r, method, input$direct_transform %||% "none", input$direct_scale %||% "as_is")
      } else comparison$selected()
    })
    export_current_pca <- pca_server("pca", selected_workflow, base_result)
    current_result <- shiny::reactive({
      apply_pca_snapshot(selected_workflow(), attr(export_current_pca, "snapshot")())
    })
    # Processing diagnostics do not require a committed normalization workflow.
    qc_result <- shiny::reactive({
      r <- tryCatch(base_result(), shiny.silent.error = function(e) NULL)
      if (is.null(r)) return(NULL)
      selected <- tryCatch(current_result(), shiny.silent.error = function(e) NULL)
      if (!is.null(selected)) return(selected)
      processing_diagnostics(r)
    })
    shiny::observe({
      r <- qc_result()
      keys <- if (is.null(r)) character() else names(r$plots) %||% character()
      selected <- shiny::isolate(input$plot_name)
      if (length(selected) != 1L || !selected %in% keys)
        selected <- if (length(keys)) keys[1L] else character()
      shiny::updateSelectInput(session, "plot_name", choices = stats::setNames(keys, analysis_text(gsub("_", " ", keys), r)), selected = selected)
    })
    show_error <- function(e) {
      result(NULL)
      notice(list(kind = "error", text = conditionMessage(e)))
      shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
    }

    shiny::observeEvent(input$inspect, {
      reviewed(NULL); reviewed_signature(NULL); approved_signature(NULL); result(NULL)
      reset_duplicate_review()
      tryCatch({
        if (is.null(input$peak_file) || is.null(input$meta_file)) stop("Select both the peak-area data and metadata files.")
        if (is.null(input$metadata_column) || !nzchar(input$metadata_column))
          stop("Select the metadata sample-ID column before reviewing inputs.")
        sig <- signature()
        sheet <- selected_metadata_sheet()
        notes <- notes_upload()
        x <- shiny::withProgress(message = "Checking input files", value = 0.2, {
          inspect_inputs(input$peak_file$datapath, input$meta_file$datapath, settings(),
            metadata_sheet = sheet, data_name = input$peak_file$name, metadata_name = input$meta_file$name,
            notes_path = if (is.null(notes)) NULL else notes$datapath,
            notes_name = if (is.null(notes)) NULL else notes$name)
        })
        if (length(x$duplicate_ids)) {
          duplicate_source(x)
          duplicate_choices(initial_duplicate_choices(x))
        }
        reviewed(x); reviewed_signature(sig)
        n <- length(x$unmatched_study_samples)
        nd <- length(x$duplicate_ids)
        notice(list(kind = if (nd || n) "warn" else "info", text = if (nd)
          paste(nd, "duplicate sample ID(s) require decisions before sample matching and processing.") else if (n)
          paste(n, "unmatched study sample(s) require review before processing.") else
          "Input checks passed. Click Run processing or Load prepared data to generate results."))
        shiny::updateTabsetPanel(session, "tabs", selected = "review")
      }, error = show_error)
    })
    shiny::observeEvent(input$approve_exclusions, {
      x <- current_review()
      shiny::req(duplicates_applied())
      if (length(x$unmatched_study_samples) && length(setdiff(x$study_sample_ids, x$unmatched_study_samples))) {
        approved_signature(approval_key())
        notice(list(kind = "info", text = paste("Exclusion approved for", length(x$unmatched_study_samples),
          "study samples in the current review. Click Run processing or Load prepared data.")))
      }
    })
    shiny::observeEvent(input$run, {
      result(NULL)
      tryCatch({
        if (is.null(reviewed()) || !identical(reviewed_signature(), signature())) stop("Click Review inputs for the current files and settings first.")
        x <- reviewed()
        if (!duplicates_applied()) stop("Resolve every duplicate sample ID and click Apply duplicate decisions before processing.")
        approved <- !is.null(approved_signature()) && identical(approved_signature(), approval_key())
        if (length(x$unmatched_study_samples) && !approved) stop("Review the unmatched study IDs and explicitly approve their exclusion, or correct the metadata and upload again.")
        r <- shiny::withProgress(message = if (settings()$processing_mode == "skip") "Loading prepared data" else paste("Processing", tolower(terms()$title), "data"), value = 0, {
          process_data(x, exclude_unmatched = approved,
            progress = function(detail) shiny::incProgress(0.1, detail = detail))
        })
        r$signature <- signature()
        r$review_revision <- review_revision()
        r$completed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        r$app_version <- "2.4.7"
        r$pathway_run_id <- input$run
        r$analysis_session_info <- utils::capture.output(utils::sessionInfo())
        r$project_title <- project_title()
        r[names(report_details())] <- report_details()
        r$data_type <- terms()$id
        r$sample_roles <- x$sample_roles
        # Processing retains metadata; PCA aligns it by sample ID, never row order.
        r$unused_metadata <- x$unused_metadata
        r$provenance <- data.frame(Field = c("Peak-area filename", "Peak-area MD5", "Metadata filename", "Metadata MD5", "Metadata sheet", "Detected orientation", "Data type", "Data sample-ID column", "Metadata sample-ID column"),
          Value = c(input$peak_file$name, unname(tools::md5sum(input$peak_file$datapath)),
            input$meta_file$name, unname(tools::md5sum(input$meta_file$datapath)),
            if (tolower(tools::file_ext(input$meta_file$name)) %in% c("xlsx", "xls")) as.character(x$metadata_sheet) else "not applicable", x$orientation, terms()$label,
            x$sample_name_column, x$metadata_name_column))
        r$provenance <- result_input_provenance(r)
        result(r)
        shiny::showNotification("Have a TCM / TBL pathway info file? Add it now or later in the PCA tab.",
          id = ns("pathway-invitation"), duration = 15, type = "message",
          action = shiny::actionLink(ns("open_pathways"), "Add pathway annotations"))
        notice(list(kind = "info", text = if (is_prepared_result(r))
          "Prepared data loaded. Processing skipped. PCA and downloads use the optional adjustments in the sidebar." else
          "Shared processing complete. Inspect QC plots, then open Compare preprocessing to calculate the comparison and commit a workflow before exporting."))
        shiny::updateTabsetPanel(session, "tabs", selected = if (is_prepared_result(r)) "pca" else "plots")
        if (!is_prepared_result(r)) session$onFlushed(function() {
          session$sendCustomMessage("metlip-processing-scroll", list(container = ns("container"), tab = "plots"))
        }, once = TRUE)
      }, error = show_error)
    })

    shiny::observeEvent(input$open_pathways, {
      shiny::updateTabsetPanel(session, "tabs", selected = "pca")
      shiny::removeNotification(ns("pathway-invitation"))
      session$onFlushed(function() session$sendCustomMessage("metlip-processing-pathways", list(container = ns("container"), panel = ns("pca-pathways-panel"))), once = TRUE)
    })

    initial_duplicate_choices <- function(x) {
      data.frame(Acquired_Sample_ID = x$duplicate_ids, Action = "exclude",
        Peak_occurrence = "", Metadata_occurrence = "",
        stringsAsFactors = FALSE, row.names = NULL)
    }
    duplicate_input_id <- function(index) paste("duplicate", review_epoch(), index, "decision", sep = "_")
    duplicate_highest_input_id <- function() paste("duplicate", review_epoch(), "highest", sep = "_")
    duplicate_list_rows <- shiny::reactive({
      x <- duplicate_source(); shiny::req(length(x$duplicate_ids))
      rows <- x$duplicate_occurrences
      repeated <- duplicated(rows[c("Acquired_Sample_ID", "Source")]) |
        duplicated(rows[c("Acquired_Sample_ID", "Source")], fromLast = TRUE)
      rows <- rows[repeated, , drop = FALSE]
      rows <- rows[order(match(rows$Acquired_Sample_ID, x$duplicate_ids),
        match(rows$Source, c("Peak-area data", "Metadata")), rows$Position), , drop = FALSE]
      rownames(rows) <- NULL
      rows
    })
    # Keep bulk and manual edits on the same invalidation and control-update path.
    update_duplicate_choices <- function(updated, ids = updated$Acquired_Sample_ID) {
      if (!identical(updated, shiny::isolate(duplicate_choices()))) {
        duplicate_choices(updated); applied_duplicate_choices(NULL)
        approved_signature(NULL); result(NULL)
        review_revision(shiny::isolate(review_revision()) + 1L)
        notice(list(kind = "warn", text = "Duplicate decisions changed. Click Apply decisions before processing."))
      }
      rows <- duplicate_list_rows()
      for (i in which(rows$Acquired_Sample_ID %in% ids)) {
        index <- match(rows$Acquired_Sample_ID[i], updated$Acquired_Sample_ID)
        field <- if (rows$Source[i] == "Peak-area data") "Peak_occurrence" else "Metadata_occurrence"
        shiny::updateSelectInput(session, duplicate_input_id(i), selected =
          if (identical(updated[index, field], rows$Occurrence_key[i])) "include" else "exclude")
      }
    }
    # Only explicit user events change decisions. Programmatic sibling updates and
    # late events from older reviews cannot overwrite the selected occurrence.
    shiny::observeEvent(input$duplicate_decision_event, {
      x <- duplicate_source(); shiny::req(review_ready(), length(x$duplicate_ids))
      rows <- duplicate_list_rows(); event <- input$duplicate_decision_event
      row <- match(event$editor_id, ns(vapply(seq_len(nrow(rows)), duplicate_input_id, character(1))))
      shiny::req(length(row) == 1L, !is.na(row), length(event$decision) == 1L,
        event$decision %in% c("include", "exclude"))
      occurrence <- rows[row, , drop = FALSE]
      index <- match(occurrence$Acquired_Sample_ID, x$duplicate_ids)
      choices <- shiny::isolate(duplicate_choices()); updated <- choices
      field <- if (occurrence$Source == "Peak-area data") "Peak_occurrence" else "Metadata_occurrence"
      other_field <- if (field == "Peak_occurrence") "Metadata_occurrence" else "Peak_occurrence"
      other <- x$duplicate_occurrences[x$duplicate_occurrences$Acquired_Sample_ID == occurrence$Acquired_Sample_ID &
        x$duplicate_occurrences$Source != occurrence$Source, , drop = FALSE]
      if (event$decision == "include") {
        updated[index, field] <- occurrence$Occurrence_key
        if (nrow(other) == 1L) updated[index, other_field] <- other$Occurrence_key
      } else if (identical(updated[index, field], occurrence$Occurrence_key)) {
        updated[index, field] <- ""
        if (nrow(other) == 1L) updated[index, other_field] <- ""
      }
      updated$Action[index] <- if (nzchar(updated$Peak_occurrence[index]) ||
        nzchar(updated$Metadata_occurrence[index])) "keep" else "exclude"
      update_duplicate_choices(updated, occurrence$Acquired_Sample_ID)
    }, priority = 90)
    shiny::observeEvent(input$duplicate_highest_event, {
      x <- duplicate_source(); event <- input$duplicate_highest_event
      shiny::req(review_ready(), length(x$duplicate_ids),
        identical(event$editor_id, ns(duplicate_highest_input_id())), isTRUE(event$checked))
      selection <- select_highest_duplicate_choices(x, shiny::isolate(duplicate_choices()))
      update_duplicate_choices(selection$choices)
      duplicate_selection_report(selection)
    }, priority = 90)
    shiny::observeEvent(input$apply_duplicates, {
      shiny::req(review_ready(), length(duplicate_source()$duplicate_ids))
      approved_signature(NULL); result(NULL); applied_duplicate_choices(NULL)
      tryCatch({
        x <- resolve_input_duplicates(duplicate_source(), duplicate_choices())
        reviewed(x)
        applied_duplicate_choices(duplicate_choices())
        review_revision(review_revision() + 1L)
        n <- length(x$unmatched_study_samples)
        notice(list(kind = if (n) "warn" else "info", text = if (n)
          paste("Duplicate decisions applied.", n, "unmatched study sample(s) require review before processing.") else
          "Duplicate decisions applied. Input checks passed. Click Run processing or Load prepared data."))
      }, error = show_error)
    })
    output$duplicate_selection_summary <- shiny::renderUI({
      selection <- duplicate_selection_report()
      if (is.null(selection)) return(NULL)
      skipped <- selection$skipped
      shiny::tagList(
        shiny::p(paste("Highest total peak area selected for", length(selection$selected_ids), "sample ID(s).")),
        if (nrow(skipped)) shiny::div(class = "notice warn",
          shiny::p("These samples were left unchanged. Review their decisions before applying; unselected samples remain excluded."),
          shiny::tags$ul(lapply(seq_len(nrow(skipped)), function(i)
            shiny::tags$li(shiny::tags$strong(skipped$Acquired_Sample_ID[i]), ": ", skipped$Reason[i])))))
    })
    output$duplicate_review <- shiny::renderUI({
      x <- duplicate_source()
      if (!length(x$duplicate_ids)) return(NULL)
      rows <- duplicate_list_rows()
      choices <- shiny::isolate(duplicate_choices())
      signal <- x$duplicate_signal
      if (is.null(signal)) signal <- duplicate_signal_summary(x)
      entries <- lapply(seq_len(nrow(rows)), function(i) {
        occurrence <- rows[i, , drop = FALSE]
        index <- match(occurrence$Acquired_Sample_ID, choices$Acquired_Sample_ID)
        field <- if (occurrence$Source == "Peak-area data") "Peak_occurrence" else "Metadata_occurrence"
        selected <- if (identical(choices[index, field], occurrence$Occurrence_key)) "include" else "exclude"
        at <- match(occurrence$Occurrence_key, signal$Occurrence_key)
        total <- signal$Total_input_signal[at]
        area <- if (occurrence$Source == "Metadata") "—" else if (!is.finite(total)) "Unavailable" else
          format(total, digits = 6L, big.mark = ",", trim = TRUE,
            scientific = total != 0 && (abs(total) < .001 || abs(total) >= 1e12))
        location <- paste(if (occurrence$Source == "Metadata") "metadata" else "peak-area", tolower(occurrence$Axis), occurrence$Position)
        if (occurrence$Source == "Peak-area data" && !is.null(x$duplicate_original$sample_batch)) {
          batch <- x$duplicate_original$sample_batch[occurrence$Index - 1L]
          if (!is.na(batch) && nzchar(batch)) location <- paste0(location, " | batch ", batch)
        }
        control <- htmltools::tagAppendAttributes(shiny::selectInput(ns(duplicate_input_id(i)), NULL,
          choices = c("Include" = "include", "Exclude" = "exclude"), selected = selected,
          selectize = FALSE, width = "100%"), class = "duplicate-row-decision")
        shiny::tags$tr(shiny::tags$td(occurrence$Acquired_Sample_ID, shiny::tags$small(style = "display:block;color:#56687c;", location)),
          shiny::tags$td(style = "text-align:right;font-variant-numeric:tabular-nums;", area), shiny::tags$td(control))
      })
      shiny::div(class = "review-section", shiny::h4("Duplicate sample IDs"),
        if (any(rows$Source == "Peak-area data")) shiny::tagList(
          shiny::div(class = "duplicate-highest-control", shiny::checkboxInput(ns(duplicate_highest_input_id()),
            "Select highest total peak area for each duplicate sample", value = FALSE)),
          shiny::helpText("Fills choices once. You can edit any decision before applying. Unchecking keeps current choices.")),
        if (any(rows$Source == "Metadata"))
          shiny::helpText("Duplicate metadata rows require manual selection; peak areas cannot choose between them."),
        shiny::uiOutput(ns("duplicate_selection_summary")),
        shiny::div(class = "duplicate-review-scroll", shiny::tags$table(class = "table table-striped duplicate-review-table",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Sample occurrence"), shiny::tags$th(style = "text-align:right;", "Total peak area"), shiny::tags$th("Decision"))),
          shiny::tags$tbody(entries))),
        shiny::actionButton(ns("apply_duplicates"), "Apply decisions", class = "btn-primary"))
    })

    output$status <- shiny::renderUI({n <- notice(); shiny::div(class = paste("notice", n$kind), n$text)})
    output$review_panel <- shiny::renderUI({
      # Do not create table containers or their headings until a review exists.
      if (!review_ready()) return(shiny::div(class = "review-empty",
        shiny::h4("Ready to review your data"),
        shiny::p("Select the peak-area and metadata files, then click Review inputs. Sample matching and previews will appear here.")))
      shiny::tagList(shiny::uiOutput(ns("notes_review")), shiny::uiOutput(ns("duplicate_review")), shiny::uiOutput(ns("resolved_input_review")))
    })
    output$notes_review <- shiny::renderUI({
      x <- current_review()
      if (analysis_terms(x)$id != "metabolomics") return(NULL)
      notes <- result_presence_filter(x)
      if (!identical(notes$status, "applied"))
        return(shiny::p(paste("Peak-picking notes:", presence_filter_status_text(x))))
      unmatched <- notes$unmatched_approved_identifiers
      shiny::tagList(shiny::div(class = "notice", shiny::strong("Peak-picking notes ready"),
        shiny::p(paste(notes$notes_name, "—", notes$counts$approved_features,
          "approved input signals. Signal selection uses totals after sample-review exclusions."))),
        if (length(unmatched)) shiny::div(class = "notice warn",
          shiny::p(paste(length(unmatched), "approved identifiers are absent from the peak-area data.")),
          shiny::tags$details(shiny::tags$summary("Unmatched approved identifiers"), table_output(ns("notes_unmatched")))))
    })
    output$notes_unmatched <- DT::renderDT({
      x <- current_review()
      table_widget(data.frame(Metabolite = result_presence_filter(x)$unmatched_approved_identifiers,
        stringsAsFactors = FALSE))
    }, server = TRUE)
    output$resolved_input_review <- shiny::renderUI({
      shiny::req(review_ready())
      if (!duplicates_applied()) return(NULL)
      shiny::tagList(
        shiny::uiOutput(ns("review_summary")),
        shiny::uiOutput(ns("exclusion_review")),
        shiny::div(class = "review-section",
          shiny::h4("Sample roles and metadata matching"), table_output(ns("sample_roles"))),
        shiny::div(class = "review-section",
          shiny::h4("Standardized data preview"),
          shiny::p(analysis_text("First 10 lipid rows and first 8 sample columns; source measurement strings are preserved here.", current_review())),
          table_output(ns("input_preview"))),
        shiny::div(class = "review-section",
          shiny::tags$details(shiny::tags$summary("Metadata IDs absent from data"), table_output(ns("unused_metadata")))))
    })
    output$review_summary <- shiny::renderUI({
      x <- current_review()
      metric <- function(value, label) shiny::div(class = "metric", shiny::strong(value), shiny::span(label))
      shiny::tagList(shiny::div(class = "metric-grid", metric(nrow(x$data), analysis_text("Input lipid features", x)),
        metric(length(x$study_sample_ids), "Study samples"), metric(length(x$blank_sample_ids), "Blanks"), metric(length(x$qc_sample_ids), "QC samples")),
        shiny::p(paste("Data type:", analysis_terms(x)$label)),
        shiny::p(paste("Detected layout:", analysis_text(gsub("_", " ", x$orientation), x), ".")))
    })
    output$exclusion_review <- shiny::renderUI({
      shiny::req(duplicates_applied())
      x <- current_review(); n <- length(x$unmatched_study_samples)
      if (!n) return(shiny::div(class = "notice", "All study sample IDs match metadata."))
      if (!length(setdiff(x$study_sample_ids, x$unmatched_study_samples))) {
        return(shiny::tagList(shiny::div(class = "notice error", "No study samples match metadata. Excluding all unmatched IDs would leave no study samples. Correct the metadata or sample IDs and upload again."),
          table_output(ns("unmatched_ids")), shiny::downloadButton(ns("unmatched_download"), "Download unmatched IDs")))
      }
      approved <- identical(approved_signature(), approval_key())
      shiny::tagList(shiny::div(class = "notice warn", shiny::strong(paste(n, "study sample IDs are absent from metadata.")),
        shiny::p("Correct the metadata and upload again, or review and approve excluding these samples from this run.")),
        table_output(ns("unmatched_ids")), shiny::div(class = "download-row", shiny::downloadButton(ns("unmatched_download"), "Download unmatched IDs"),
        if (!approved) shiny::actionButton(ns("approve_exclusions"), paste("Approve excluding these", n, "samples"), class = "btn-warning") else
          shiny::span("Exclusion approved for the current files and settings.")))
    })
    table_widget <- function(x) DT::datatable(x, rownames = FALSE, escape = TRUE, fillContainer = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, lengthChange = FALSE))
    output$sample_roles <- DT::renderDT({table_widget(current_review()$sample_roles)}, server = TRUE)
    output$unmatched_ids <- DT::renderDT({table_widget(data.frame(Acquired_Sample_ID = current_review()$unmatched_study_samples))}, server = TRUE)
    output$unused_metadata <- DT::renderDT({table_widget(data.frame(Acquired_Sample_ID = current_review()$unused_metadata))}, server = TRUE)
    output$input_preview <- DT::renderDT({x <- current_review()$data; table_widget(export_feature_table(
      x[seq_len(min(10, nrow(x))), seq_len(min(9, ncol(x))), drop = FALSE], current_review(), "Lipid"))}, server = TRUE)
    output$unmatched_download <- shiny::downloadHandler(filename = function() metlip_download_name("unmatched_sample_IDs.csv", report_details()$metlip_id), content = function(file) {
      shiny::req(duplicates_applied())
      utils::write.csv(data.frame(Acquired_Sample_ID = current_review()$unmatched_study_samples), file, row.names = FALSE)
    })
    output$qc_plot_status <- shiny::renderUI({
      r <- qc_result()
      if (is.null(r)) return(shiny::div(class = "notice", "Run processing to see QC plots."))
      if (is_prepared_result(r)) return(shiny::p("Processing skipped. Only available PCA figures are shown; no processing or normalization-comparison diagnostics were calculated."))
      if (is.null(r$selection)) return(shiny::p("Showing processing diagnostics. Commit a workflow in Compare preprocessing to add normalization and PCA plots."))
      shiny::p(paste("Processing diagnostics and committed workflow:", normalization_labels[[r$selection$method]],
        "|", r$selection$transform, "|", scaling_labels[[r$selection$scaling]]))
    })
    selected_qc_plot <- shiny::reactive({
      r <- qc_result()
      shiny::req(!is.null(r), length(input$plot_name) == 1L, input$plot_name %in% names(r$plots))
      list(name = input$plot_name, plot = r$plots[[input$plot_name]], size = r$plot_sizes[[input$plot_name]])
    })
    output$plot_panel <- shiny::renderUI({
      p <- selected_qc_plot()
      figure <- shiny::plotOutput(ns("qc_plot"), height = "800px")
      if (grepl("^pca_", p$name)) legend_figure_ui(figure, ns("qc_legend_ui"), "800px") else figure
    })
    output$qc_plot <- shiny::renderPlot({
      p <- selected_qc_plot()
      if (grepl("^pca_", p$name)) plot_without_legend(p$plot) else p$plot
    }, res = 110)
    bind_plot_legend(output, session, "qc", shiny::reactive({
      p <- selected_qc_plot(); shiny::req(grepl("^pca_", p$name)); p$plot
    }))
    output$plot_download <- shiny::renderUI({selected_qc_plot(); shiny::downloadButton(ns("download_plot"), "Download this plot (PDF)")})
    output$download_plot <- shiny::downloadHandler(filename = function() {
      name <- selected_qc_plot()$name
      if (analysis_terms(qc_result())$id == "metabolomics") name <- gsub("lipid", "metabolite", name, fixed = TRUE)
      metlip_download_name(paste0(name, ".pdf"), qc_result()$metlip_id)
    }, content = function(file) {
      p <- selected_qc_plot()
      save_result_pdf(file, plot = p$plot, width = p$size$width,
        height = p$size$height, pca = grepl("^pca_", p$name))
    })
    result_tables <- shiny::reactive(result_table_catalog(current_result()))
    output$results_panel <- shiny::renderUI({
      r <- tryCatch(current_result(), shiny.silent.error = function(e) NULL)
      if (is.null(r)) {
        processed <- !is.null(tryCatch(base_result(), shiny.silent.error = function(e) NULL))
        return(shiny::div(class = "review-empty",
          shiny::h4(if (processed) "Choose your final workflow" else "Your results will appear here"),
          shiny::p(if (processed) paste("Open Compare preprocessing, calculate the comparison, then click",
            "Use this workflow for PCA and exports to commit your choices. If you change those choices, commit again to refresh downloads.") else
            "Upload your peak-area and metadata files, review the inputs, and run processing. Then compare and commit a workflow to enable downloads."),
          if (processed) shiny::div(class = "download-row", shiny::actionButton(ns("results_go_compare"), "Compare preprocessing", class = "btn-primary"))))
      }
      catalog <- result_tables()
      prepared <- is_prepared_result(r)
      study_dimensions <- if (prepared) dim(r$supplied_study) else c(ncol(r$normalized_wide) - 1L, nrow(r$normalized_wide))
      groups <- unique(vapply(catalog, `[[`, character(1), "group"))
      choices <- stats::setNames(lapply(groups, function(group) {
        entries <- catalog[vapply(catalog, function(entry) identical(entry$group, group), logical(1))]
        stats::setNames(names(entries), vapply(entries, `[[`, character(1), "label"))
      }), groups)
      metric <- function(value, label) shiny::div(class = "metric", shiny::strong(format(value, big.mark = ",", trim = TRUE)), shiny::span(label))
      shiny::tagList(
        shiny::div(class = "results-heading", shiny::h3("Results"), shiny::span(class = "results-badge", if (prepared) "Processing skipped" else "Committed workflow")),
        shiny::p(class = "results-meta", r$project_title, shiny::br(), paste("Processed:", r$completed_at),
          " · ", paste("Workflow committed:", r$selection$selected_at)),
        shiny::div(class = "workflow-strip",
          shiny::span(analysis_terms(r)$label),
          shiny::span(paste("Normalization:", normalization_labels[[r$selection$method]])),
          shiny::span(paste("Transformation:", if (r$selection$transform == "log2") "Log2" else "None")),
          shiny::span(paste("Scaling:", scaling_labels[[r$selection$scaling]]))),
        shiny::div(class = "metric-grid", metric(study_dimensions[1], "Study samples"),
          metric(study_dimensions[2], analysis_text("Retained lipids", r)),
          metric(if (prepared) nrow(r$supplied_qc) else length(r$comparison$evidence[[r$selection$method]]$usable_qc), "Eligible QC samples")),
        shiny::p(paste("Peak-picking notes:", presence_filter_status_text(r)),
          if (identical(result_presence_filter(r)$status, "applied")) {
            counts <- result_presence_filter(r)$counts
            paste0(". ", counts$excluded_not_approved, " signals excluded by notes; ",
              counts$collapsed_duplicates, " additional approved signals collapsed.")
          }),
        if (inherits(r$selected_pca, "error")) shiny::div(class = "notice warn",
          paste("PCA unavailable:", conditionMessage(r$selected_pca)),
          shiny::p(if (prepared) "Supplied measurements and the report remain available. Correct missing data or select compatible adjustments for PCA." else "Normalized tables, QC results and the report remain available.")),
        if (is.list(r$selected_pca) && !inherits(r$selected_pca, "error"))
          shiny::p(paste("Final PCA:", r$selected_pca$settings$sample_scope)),
        if (length(r$pca_plot_errors)) shiny::div(class = "notice warn",
          shiny::p("Some PCA figures are unavailable. Review the color/shape and display settings in the PCA tab."),
          lapply(r$pca_plot_errors, p)),
        shiny::div(class = "export-grid",
          shiny::div(class = "export-card", shiny::h4("Result tables"),
            shiny::p("Normalized data, QC evidence, PCA tables and run settings grouped into folders. Includes an index with file paths and descriptions."),
            shiny::downloadButton(ns("download_tables"), "All tables (CSV ZIP)", class = "btn-primary"),
            shiny::p("Use the table browser below for individual CSV or TSV files.")),
          shiny::div(class = "export-card", shiny::uiOutput(ns("report_controls"))),
          shiny::div(class = "export-card", shiny::h4("Complete archive"),
            shiny::p("Results, audit data, plots, PCA models and report source organized by folder, with a guide to the contents. Includes HTML when report rendering is available."),
            shiny::downloadButton(ns("download_all"), "Full results (ZIP)"))),
        shiny::div(class = "results-section", shiny::h4("Browse and download tables"),
          shiny::div(class = "table-toolbar",
            shiny::selectInput(ns("result_table"), "Table", choices = choices, selected = if ("normalized_wide" %in% names(catalog)) "normalized_wide" else "supplied_wide", width = "100%"),
            shiny::div(class = "download-row", shiny::downloadButton(ns("download_table_csv"), "CSV"), shiny::downloadButton(ns("download_table_tsv"), "TSV"))),
          shiny::uiOutput(ns("result_table_description")), table_output(ns("result_preview")),
          shiny::helpText("Search and paging affect the preview only. Downloads always include every row and column of the selected table.")),
        shiny::tags$details(class = "results-details", shiny::tags$summary("Run details and messages"),
          shiny::h4("Selection rationale"), shiny::p(r$selection$rationale),
          shiny::h4("Processing summary"), shiny::tableOutput(ns("run_summary")),
          shiny::h4("Settings used for this result"), shiny::tableOutput(ns("settings_used")),
          shiny::h4("Run messages"), shiny::verbatimTextOutput(ns("run_messages"))))
    })
    shiny::observeEvent(input$results_go_compare, shiny::updateTabsetPanel(session, "tabs", selected = "compare"))
    selected_result_table <- shiny::reactive({
      catalog <- result_tables()
      key <- input$result_table %||% "normalized_wide"
      shiny::validate(shiny::need(length(key) == 1L && key %in% names(catalog), "Choose an available result table."))
      catalog[[key]]
    })
    output$result_table_description <- shiny::renderUI({
      entry <- selected_result_table()
      shiny::div(class = "table-description", shiny::p(entry$description),
        shiny::p(paste(format(nrow(entry$data), big.mark = ",", trim = TRUE), "rows ·",
          format(ncol(entry$data), big.mark = ",", trim = TRUE), "columns")))
    })
    report_setup <- shiny::reactive({
      input$check_report_setup
      input$recheck_report_startup
      if (!file.exists(metlip_report_template())) return(list(ready = FALSE,
        message = "Report template is missing. Reinstall MetLipAutomation with its processing report resources."))
      report_requirements()
    })
    output$report_setup_status <- shiny::renderUI({
      setup <- report_setup()
      if (setup$ready) return(shiny::p(class = "help-block", paste0("HTML reports ready",
        if (!is.null(setup$version)) paste0(" (Quarto ", setup$version, ")") else "", ".")))
      shiny::div(class = "notice warn", shiny::strong("HTML report setup needs attention"), shiny::p(setup$message),
        shiny::tags$a(href = "https://quarto.org/docs/get-started/", target = "_blank", "Quarto installation instructions"),
        shiny::div(class = "download-row", shiny::actionButton(ns("recheck_report_startup"), "Recheck report setup")))
    })
    output$report_controls <- shiny::renderUI({
      current_result()
      setup <- report_setup()
      shiny::tagList(shiny::h4("HTML report"),
        shiny::p("A standalone Quarto report with the committed workflow, QC evidence, PCA overview and figures. Opens in a browser."),
        if (setup$ready) shiny::downloadButton(ns("download_report"), "Quarto report (HTML)", class = "btn-primary") else
          shiny::tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Quarto report (HTML)"),
        shiny::tags$details(shiny::tags$summary("Editable report source"),
          shiny::p("Includes the Quarto document, data snapshot and figures for editing or rendering later."),
          shiny::downloadButton(ns("download_report_source"), "Report source (ZIP)")),
        if (!setup$ready) shiny::div(class = "notice warn", setup$message,
          shiny::actionButton(ns("check_report_setup"), "Recheck report setup")))
    })
    output$run_summary <- shiny::renderTable(current_result()$summary, striped = TRUE)
    output$settings_used <- shiny::renderTable(rbind(settings_table(current_result()$settings), settings_table(current_result()$selection)), striped = TRUE)
    output$result_preview <- DT::renderDT(table_widget(selected_result_table()$data), server = TRUE)
    output$run_messages <- shiny::renderText(paste(current_result()$messages, collapse = "\n"))
    output$download_table_csv <- shiny::downloadHandler(filename = function() metlip_download_name(selected_result_table()$filename, current_result()$metlip_id),
      content = function(file) write_result_table(selected_result_table(), file, "csv"), contentType = "text/csv")
    output$download_table_tsv <- shiny::downloadHandler(filename = function() metlip_download_name(sub("\\.csv$", ".tsv", selected_result_table()$filename), current_result()$metlip_id),
      content = function(file) write_result_table(selected_result_table(), file, "tsv"), contentType = "text/tab-separated-values")
    output$download_tables <- shiny::downloadHandler(filename = function() metlip_download_name(paste0(analysis_terms(current_result())$file_prefix, "_tables_", format(Sys.Date(), "%Y%m%d"), ".zip"), current_result()$metlip_id),
      content = function(file) {
        r <- current_result()
        folder <- tempfile(paste0(analysis_terms(r)$file_prefix, "_tables_"))
        dir.create(folder)
        on.exit(unlink(folder, recursive = TRUE), add = TRUE)
        shiny::withProgress(message = "Preparing result tables", value = 0.2, {
          write_result_tables_bundle(r, folder)
          write_zip_bundle(file, folder)
        })
      }, contentType = "application/zip")
    report_download_error <- function(e) {
      shiny::showNotification(paste("Report download failed:", conditionMessage(e)), type = "error", duration = 15)
      stop(e)
    }
    output$download_report <- shiny::downloadHandler(filename = function() metlip_download_name(paste0(analysis_terms(current_result())$file_prefix, "_report.html"), current_result()$metlip_id), content = function(file) {
      r <- current_result()
      folder <- tempfile(paste0(analysis_terms(r)$file_prefix, "_report_"))
      dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      tryCatch(shiny::withProgress(message = "Creating HTML report", value = 0.1, {
        write_report_source(r, folder, metlip_report_template())
        shiny::incProgress(0.4, detail = "Rendering the completed run")
        html <- render_report(folder, output_name = metlip_download_name(paste0(analysis_terms(r)$file_prefix, "_report.html"), r$metlip_id))
        if (!file.copy(html, file, overwrite = TRUE)) stop("Could not prepare the report download.")
      }), error = report_download_error)
    }, contentType = "text/html")
    output$download_report_source <- shiny::downloadHandler(filename = function() metlip_download_name(paste0(analysis_terms(current_result())$file_prefix, "_report_source.zip"), current_result()$metlip_id), content = function(file) {
      r <- current_result()
      folder <- tempfile(paste0(analysis_terms(r)$file_prefix, "_report_source_"))
      dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      tryCatch(shiny::withProgress(message = "Preparing editable report", value = 0.2, {
        write_report_source(r, folder, metlip_report_template())
        write_zip_bundle(file, folder)
      }), error = report_download_error)
    }, contentType = "application/zip")
    output$download_all <- shiny::downloadHandler(filename = function() metlip_download_name(paste0(analysis_terms(current_result())$file_prefix, "_results_", format(Sys.Date(), "%Y%m%d"), ".zip"), current_result()$metlip_id), content = function(file) {
      r <- current_result()
      folder <- tempfile(paste0(analysis_terms(r)$file_prefix, "_export_"))
      dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      shiny::withProgress(message = "Creating results and report bundle", value = 0.1, {
        write_results_bundle(r, folder)
        export_current_pca(file.path(folder, "PCA"))
        report_dir <- file.path(folder, "report")
        shiny::incProgress(0.3, detail = "Preparing the report source and figures")
        # The ordinary data/PDF bundle remains available if report rendering fails.
        report_error <- tryCatch({
          write_report_source(r, report_dir, metlip_report_template())
          setup <- report_requirements()
          if (!setup$ready) stop(setup$message)
          shiny::incProgress(0.3, detail = "Rendering HTML")
          render_report(report_dir, output_name = metlip_download_name(paste0(analysis_terms(r)$file_prefix, "_report.html"), r$metlip_id))
          NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(report_error)) {
          dir.create(report_dir, showWarnings = FALSE)
          # Never package a partially rendered HTML file as a completed report.
          unlink(file.path(report_dir, metlip_download_name(paste0(analysis_terms(r)$file_prefix, "_report.html"), r$metlip_id)))
          writeLines(c("HTML report was not generated.", report_error,
            "The data and plot exports in this ZIP are from the completed run.",
            "If report.qmd, report_data.rds and figures are present, the report can be rendered after resolving the setup issue."),
            file.path(report_dir, "REPORT_STATUS.txt"))
          shiny::showNotification(paste("Results exported; HTML report unavailable:", report_error), type = "warning", duration = 15)
        }
      })
      write_zip_bundle(file, folder)
    }, contentType = "application/zip")
    # Read-only reactive interface for composition and module acceptance checks.
    list(result = current_result, review = current_review,
      status = shiny::reactive(notice()), comparison = comparison$comparison)
  })
}
