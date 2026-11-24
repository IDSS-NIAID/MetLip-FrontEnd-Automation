comparison_ui <- function(id, processing_ns = identity) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::conditionalPanel("input.processing_mode === 'skip'", ns = processing_ns,
      shiny::div(class = "notice", "Comparison is skipped for prepared data. Use the optional PCA adjustments in the sidebar.")),
    shiny::conditionalPanel("input.processing_mode !== 'skip'", ns = processing_ns,
    shiny::h3("Compare preprocessing"),
    shiny::p("Compare methods on identical retained features and shared imputation. Choose a workflow explicitly before exporting."),
    shiny::actionButton(ns("compare"), "Calculate comparison", class = "btn-primary"),
    shiny::helpText("PQN uses the study-sample median reference and does not require QCs. Identified QCs are evaluated together as one assumed homogeneous pool across batches, with no manual exclusions. Fixed QC criteria: 80% detection per sample and feature, at least 3 observed measurements per feature, and a 30% RSD threshold."),
    shiny::uiOutput(ns("status")),
    shiny::h4("1. Normalization evidence"), shiny::uiOutput(ns("evidence")),
    shiny::h4("2. Inspect the same samples and features across workflows"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("transform"), "Transformation for PCA", c("Log2" = "log2", "None" = "none"), selected = "none")),
      shiny::column(4, shiny::selectInput(ns("color"), "Color by metadata", choices = c("None" = "")),
        shiny::selectInput(ns("color_mode"), "Color type", c("Automatic" = "auto", "Categorical" = "categorical", "Continuous numeric" = "continuous"))),
      shiny::column(4, shiny::selectInput(ns("scope"), "Samples shown", c("Studies + eligible QCs" = "all", "Studies" = "study", "Eligible QCs" = "qc")))),
    shiny::helpText("After processing, every metadata column after the sample-ID column is available for coloring, including names with spaces. Automatic uses a numeric gradient when all observed values are numeric; otherwise it uses categories. Choose Categorical for numeric group codes. Missing metadata appear in grey."),
    shiny::helpText("Filters sample plots, including PCA scores and pairs. PCA stays fitted on study samples; loadings, explained variance and QC RSD do not change with this filter."),
    shiny::selectInput(ns("plot"), "Diagnostic", c("PCA scores" = "scores", "PCA loadings" = "loadings", "PCA pairs" = "pairs",
      "Explained variance" = "scree", "Density" = "density", "RLE" = "rle", "Distributions" = "distribution", "QC RSD" = "qc_rsd",
      "Feature by sample" = "feature")),
    shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("sample"), "Highlight sample", choices = c("None" = ""))),
      shiny::column(6, shiny::selectInput(ns("feature"), "Inspect feature", choices = character()))),
    shiny::checkboxInput(ns("observed"), "Observed-only distribution and feature plots (PCA still uses shared imputation)", FALSE),
    shiny::helpText("Drag a box to zoom. Double-click or click Reset zoom to restore the full view. For plots with multiple panels, zoom focuses the selected panel."),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(ns("left_method"), "Left normalization", stats::setNames(names(normalization_labels), normalization_labels), selected = "tsn"),
        shiny::selectInput(ns("left_scale"), "Left scaling", stats::setNames(names(scaling_labels), scaling_labels), selected = "unit"),
        legend_figure_ui(htmltools::tagAppendAttributes(shiny::plotOutput(ns("left_plot"), height = "540px", click = ns("left_click"),
          dblclick = ns("left_dblclick"), brush = shiny::brushOpts(ns("left_brush"), resetOnNew = TRUE)),
          `data-zoom-complete` = ns("left_brush_end")), ns("left_legend_ui"), "540px"),
        shiny::div(class = "download-row",
          shiny::actionButton(ns("left_reset"), "Reset zoom"))),
      shiny::column(6, shiny::selectInput(ns("right_method"), "Right normalization", stats::setNames(names(normalization_labels), normalization_labels), selected = "tsn"),
        shiny::selectInput(ns("right_scale"), "Right scaling", stats::setNames(names(scaling_labels), scaling_labels), selected = "pareto"),
        legend_figure_ui(htmltools::tagAppendAttributes(shiny::plotOutput(ns("right_plot"), height = "540px", click = ns("right_click"),
          dblclick = ns("right_dblclick"), brush = shiny::brushOpts(ns("right_brush"), resetOnNew = TRUE)),
          `data-zoom-complete` = ns("right_brush_end")), ns("right_legend_ui"), "540px"),
        shiny::div(class = "download-row",
          shiny::actionButton(ns("right_reset"), "Reset zoom")))),
    shiny::helpText("Density, RLE, distributions, and QC RSD evaluate normalization independently of scaling. PCA panels use their own study-fitted basis; PC coordinates and distances are not directly comparable. PCA is exploratory, not a significance test. Inspect other axes in the committed PCA tab."),
    shiny::tags$details(shiny::tags$summary("Scaling sensitivity across normalization and scaling choices"), shiny::uiOutput(ns("scaling_evidence")),
      shiny::p("PC1 component percentage and squared loadings describe feature influence, not quality. High influence from poorly observed or noisy features warrants investigation. Scaling cannot improve per-feature QC RSD.")),
    shiny::downloadButton(ns("plots_download"), "Download comparison plots and scorecards (ZIP)"),
    shiny::h4("3. Commit the export workflow"),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(ns("selected_method"), "Normalization", stats::setNames(names(normalization_labels), normalization_labels), selected = "none")),
      shiny::column(6, shiny::selectInput(ns("selected_scale"), "Feature scaling", stats::setNames(names(scaling_labels), scaling_labels), selected = "as_is"))),
    shiny::textAreaInput(ns("rationale"), "Selection rationale / limitations", width = "100%", rows = 3,
      placeholder = "Record QC evidence, unresolved technical effects, and the assumptions supporting this choice."),
    shiny::actionButton(ns("commit"), "Use this workflow for PCA and exports", class = "btn-primary"),
    shiny::uiOutput(ns("selection_status")),
    shiny::tags$details(shiny::tags$summary("Methods, assumptions and evidence"),
      shiny::p("TSN: divide by sample sum and multiply by mean study sum. Median: divide by sample median and multiply by median study median. PQN: common-total prescaling, feature-wise median reference from study samples, median feature/reference quotient, then divide the prescaled sample by that quotient."),
      shiny::p("Factors are never fitted separately by biological group. Study samples define centering and SDs; QCs are projected. Z-scaling divides by feature SD, Pareto by sqrt(SD), after optional log2. Constant features are omitted and exported. PCA computation does not require normality."),
      shiny::p("A global biological increase in feature abundance can be removed by TSN or median normalization. Tight QC clusters and aligned distributions do not prove biological preservation. Sample factors cannot generally remove feature-specific drift. Inspect controls and feature plots."),
      shiny::p("No predictive validation is performed. Future prediction models must learn preprocessing within training folds."),
      shiny::tags$a(href = "https://doi.org/10.1021/ac051632c", target = "_blank", "Dieterle et al. (PQN)"), " | ",
      shiny::tags$a(href = "https://doi.org/10.1186/1471-2164-7-142", target = "_blank", "van den Berg et al. (scaling)"), " | ",
      shiny::tags$a(href = "https://doi.org/10.1007/s11306-018-1367-3", target = "_blank", "Broadhurst et al. (QC)"))))
}

comparison_server <- function(id, base_result) {
  shiny::moduleServer(id, function(input, output, session) {
    cmp <- shiny::reactiveVal(NULL); committed <- shiny::reactiveVal(NULL)
    zoom <- shiny::reactiveValues(left = NULL, right = NULL)
    brushes <- shiny::reactiveValues(left = NULL, right = NULL)
    zoom_generation <- shiny::reactiveValues(left = 0L, right = 0L)
    clear_brush <- function(side) {
      brushes[[side]] <- NULL
      zoom_generation[[side]] <- zoom_generation[[side]] + 1L
      session$sendCustomMessage("plot-zoom-cancel", list(
        id = session$ns(paste0(side, "_brush_end")), generation = zoom_generation[[side]]))
      session$resetBrush(session$ns(paste0(side, "_brush")))
    }
    clear_zoom <- function(side) {
      zoom[[side]] <- NULL
      clear_brush(side)
    }
    config <- comparison_config()
    available_base <- shiny::reactive(tryCatch(base_result(), shiny.silent.error = function(e) NULL))
    shiny::observeEvent(available_base(), {
      cmp(NULL); committed(NULL)
      r <- available_base()
      metadata <- if (is.null(r)) NULL else r$metadata
      if (!is.null(r)) {
        if (is.null(metadata)) metadata <- data.frame(Acquired_Sample_ID = character())
        metadata$.Role <- rep(NA_character_, nrow(metadata))
        metadata$.Imputed_fraction <- rep(NA_real_, nrow(metadata))
      }
      selected <- metadata_color_selection(metadata, shiny::isolate(input$color), ".Role")
      shiny::updateSelectInput(session, "color", choices = metadata_color_choices(metadata), selected = selected)
    }, ignoreNULL = FALSE, priority = 50)
    shiny::observeEvent(input$compare, {
      committed(NULL); cmp(NULL)
      tryCatch({
        if (is_prepared_result(base_result())) stop("Comparison is skipped for prepared data; use the direct PCA controls.")
        c <- shiny::withProgress(message = "Comparing normalization methods", value = .3, run_comparison(base_result(), config))
        cmp(c)
        shiny::updateSelectInput(session, "sample", choices = c("None" = "", stats::setNames(rownames(c$input$x), rownames(c$input$x))))
        shiny::updateSelectInput(session, "feature", label = paste("Inspect", analysis_terms(c)$feature), choices = colnames(c$input$x))
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 15))
    })
    current <- shiny::reactive({
      base_result(); c <- cmp()
      shiny::validate(shiny::need(!is.null(c), "Calculate the comparison to see results."))
      c
    })
    # htmlwidgets renders validation errors as positioned overlays. Keep empty
    # states in normal document flow and mount tables only when data is ready.
    display_comparison <- shiny::reactive(tryCatch(current(), shiny.silent.error = function(e) NULL))
    output$evidence <- shiny::renderUI({
      if (is.null(display_comparison())) return(shiny::div(class = "review-empty",
        shiny::p("Click Calculate comparison to view normalization evidence for the processed data.")))
      shiny::div(class = "comparison-evidence",
        table_output(session$ns("scorecard")),
        shiny::helpText("NA means unavailable. Each eligible feature has equal weight. QC RSD uses observed values before log/centering/scaling; sample factors use shared imputation. No automatic winner, hypothesis tests, or group-separation optimization."),
        shiny::tags$details(shiny::tags$summary("Feature-level QC evidence (right method)"), table_output(session$ns("qc_table"))))
    })
    output$scaling_evidence <- shiny::renderUI({
      if (is.null(display_comparison())) return(shiny::p("Calculate the comparison to view scaling sensitivity."))
      table_output(session$ns("scaling_table"))
    })
    output$status <- shiny::renderUI({
      c <- display_comparison()
      if (is.null(c)) return(NULL)
      messages <- character()
      if (!length(c$input$qc)) messages <- c(messages, "No pooled QCs identified: technical reproducibility cannot be established.")
      if (length(c$input$qc) && !any(is.finite(c$scorecard$Median_QC_RSD_percent)))
        messages <- c(messages, "QC reproducibility is unavailable: too few observed measurements meet the fixed detection criteria. Other comparisons remain available.")
      if (length(messages)) shiny::div(class = "notice", lapply(messages, shiny::p)) else NULL
    })
    widget <- function(x) DT::datatable(x, rownames = FALSE, escape = TRUE, fillContainer = FALSE,
      options = list(scrollX = TRUE, pageLength = 6))
    output$scorecard <- DT::renderDT({
      c <- display_comparison(); shiny::req(!is.null(c))
      widget(comparison_scorecard_view(c))
    }, server = TRUE)
    output$qc_table <- DT::renderDT({
      c <- display_comparison(); shiny::req(!is.null(c))
      shiny::validate(normalization_method_error(input$right_method))
      qc <- c$evidence[[input$right_method]]$qc
      widget(export_feature_table(qc[, setdiff(names(qc), c("Pool", "Batch")), drop = FALSE], c, "Lipid"))
    }, server = TRUE)
    scales <- shiny::reactive(scaling_scorecard(current(), input$transform))
    output$scaling_table <- DT::renderDT({
      shiny::req(!is.null(display_comparison()))
      widget(scales())
    }, server = TRUE)
    left_fit <- shiny::reactive(tryCatch(fit_lipid_pca(candidate_result(current(), input$left_method, input$transform, input$left_scale),
      "tsn", input$transform, input$left_scale), error = identity))
    right_fit <- shiny::reactive(tryCatch(fit_lipid_pca(candidate_result(current(), input$right_method, input$transform, input$right_scale),
      "tsn", input$transform, input$right_scale), error = identity))
    source_plot <- function(side) {
      method <- if (side == "left") input$left_method else input$right_method
      shiny::validate(normalization_method_error(method))
      comparison_plot(current(), method,
        type = input$plot, fit = if (side == "left") left_fit() else right_fit(), color = input$color,
        sample = input$sample, feature = input$feature, scope = input$scope, observed = isTRUE(input$observed), color_mode = input$color_mode %||% "auto")
    }
    draw <- function(side) comparison_zoom_plot(source_plot(side), zoom[[side]])
    # Return ggplot objects so Shiny retains the data coordinates used by nearPoints.
    output$left_plot <- shiny::renderPlot({
      p <- draw("left")
      if (input$plot %in% c("scores", "loadings", "pairs")) plot_without_legend(p) else p
    }, res = 100)
    output$right_plot <- shiny::renderPlot({
      p <- draw("right")
      if (input$plot %in% c("scores", "loadings", "pairs")) plot_without_legend(p) else p
    }, res = 100)
    for (side in c("left", "right")) local({
      s <- side
      bind_plot_legend(output, session, s, shiny::reactive({
        shiny::req(input$plot %in% c("scores", "loadings", "pairs"))
        draw(s)
      }))
      # Capture first so a simultaneous settings change can discard the old brush.
      shiny::observeEvent(input[[paste0(s, "_brush_end")]], {
        brushes[[s]] <- input[[paste0(s, "_brush_end")]]
      }, ignoreNULL = FALSE, priority = 30)
      # Keep each side independent; discard limits when the plotted data or axes change.
      shiny::observeEvent(list(tryCatch(base_result(), error = function(e) NULL), input$compare,
        input$plot, input$scope, input$transform, input$observed, input$feature,
        input[[paste0(s, "_method")]], input[[paste0(s, "_scale")]]), {
        clear_zoom(s)
      }, ignoreNULL = FALSE, priority = 20)
      shiny::observeEvent(input[[paste0(s, "_brush_end")]], {
        bounds <- plot_zoom_event_bounds(brushes[[s]], zoom_generation[[s]])
        if (is.null(bounds)) return()
        p <- source_plot(s)
        if (!is.data.frame(p$data) || !nrow(p$data)) return()
        # Focused facets no longer send facet metadata with later brushes.
        if (!length(bounds$facets) && !is.null(zoom[[s]])) bounds$facets <- zoom[[s]]$facets
        zoom[[s]] <- bounds
        clear_brush(s)
      })
      shiny::observeEvent(input[[paste0(s, "_reset")]], clear_zoom(s), priority = 20)
      shiny::observeEvent(input[[paste0(s, "_dblclick")]], clear_zoom(s), priority = 20)
      shiny::observeEvent(input[[paste0(s, "_click")]], {
        if (input$plot != "scores") return()
        p <- draw(s); if (!all(c(".x", ".y") %in% names(p$data))) return()
        near <- shiny::nearPoints(p$data, input[[paste0(s, "_click")]], xvar = ".x", yvar = ".y", maxpoints = 1)
        if (nrow(near)) shiny::updateSelectInput(session, "sample", selected = near$Acquired_Sample_ID[1])
      })
    })
    selection_key <- shiny::reactive(list(method = input$selected_method, scaling = input$selected_scale,
      transform = input$transform, rationale = trimws(input$rationale %||% "")))
    shiny::observeEvent(input$commit, {
      tryCatch({
        c <- current(); key <- selection_key()
        if (!nzchar(key$rationale)) stop("Enter a selection rationale, including evidence limitations.")
        r <- selected_diagnostics(candidate_result(c, key$method, key$transform, key$scaling, key$rationale))
        r$scaling_scorecard <- scales()
        committed(list(result = r, key = key))
        shiny::showNotification("Workflow committed. PCA and downloads now use these settings.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12))
    })
    selected <- shiny::reactive({
      current(); record <- committed()
      shiny::validate(normalization_method_error(input$selected_method))
      shiny::validate(shiny::need(!is.null(record), "Commit a workflow in Compare preprocessing before exporting."),
        shiny::need(identical(record$key, selection_key()), "Choices changed. Commit the current workflow before exporting."))
      record$result
    })
    output$selection_status <- shiny::renderUI({
      r <- selected()
      shiny::div(class = "notice", paste("Committed:", normalization_labels[[r$selection$method]], "|", r$selection$transform,
        "|", scaling_labels[[r$selection$scaling]]), shiny::p(r$selection$rationale),
        if (inherits(r$selected_pca, "error")) shiny::p(paste("PCA unavailable:", conditionMessage(r$selected_pca))))
    })
    output$plots_download <- shiny::downloadHandler(filename = function() metlip_download_name("preprocessing_comparison.zip", base_result()$metlip_id), content = function(file) {
      c <- current(); folder <- tempfile("comparison_"); dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      write_comparison_bundle(c, folder, input$transform)
      save_result_pdf(export_file(folder, "plots", "left_view.pdf"), draw("left"), width = 11, height = 8,
        pca = input$plot %in% c("scores", "loadings", "pairs"))
      save_result_pdf(export_file(folder, "plots", "right_view.pdf"), draw("right"), width = 11, height = 8,
        pca = input$plot %in% c("scores", "loadings", "pairs"))
      zoom_settings <- function(side) {
        z <- zoom[[side]]
        stats::setNames(c(z$xlim %||% c("", ""), z$ylim %||% c("", ""),
          paste(names(z$facets), unlist(z$facets), sep = "=", collapse = "; ")),
          paste0(side, c("_zoom_xmin", "_zoom_xmax", "_zoom_ymin", "_zoom_ymax", "_zoom_facets")))
      }
      view_zoom <- c(zoom_settings("left"), zoom_settings("right"))
      color <- metadata_color_selection(c$input$metadata, input$color, ".Role")
      color_mode <- input$color_mode %||% "auto"
      resolved_mode <- tryCatch({
        metadata <- if (input$plot %in% c("scores", "pairs")) left_fit()$metadata else c$input$metadata
        metadata_color_spec(metadata, color, color_mode)$mode
      }, error = function(e) "unavailable")
      utils::write.csv(data.frame(Field = c("diagnostic", "left_method", "left_scaling", "right_method", "right_scaling", "transform", "color", "color_mode", "color_mode_resolved", "sample", "feature", "scope", "observed_only", names(view_zoom)),
        Value = c(input$plot, input$left_method, input$left_scale, input$right_method, input$right_scale, input$transform, color, color_mode, resolved_mode, input$sample, input$feature, input$scope, isTRUE(input$observed), unname(view_zoom))),
        file.path(folder, "plots", "view_settings.csv"), row.names = FALSE)
      write_zip_bundle(file, folder)
    })
    list(selected = selected, comparison = current)
  })
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x
