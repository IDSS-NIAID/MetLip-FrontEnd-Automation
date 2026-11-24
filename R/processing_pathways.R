# Optional feature annotations. These helpers never alter measurements or fit PCA.
pathway_features <- function(result) {
  if (is.null(result)) return(data.frame(Lipid = character(), Lipid_base = character()))
  prepared <- identical(result$settings$processing_mode, "skip")
  ids <- if (prepared && !is.null(result$supplied_wide)) result$supplied_wide$Lipid else
    if (!is.null(result$imputed)) unique(result$imputed$Lipid) else unique(result$normalized_long$Lipid)
  ids <- as.character(ids)
  base <- ids
  key <- result$lipid_name_key
  if (!prepared && is.data.frame(key) && "Lipid_base" %in% names(key)) {
    at <- match(ids, key$Lipid)
    known <- !is.na(at) & !is.na(key$Lipid_base[at])
    base[known] <- as.character(key$Lipid_base[at[known]])
  }
  data.frame(Lipid = ids, Lipid_base = base, stringsAsFactors = FALSE)
}

read_pathway_table <- function(path, name = basename(path), delimiter = "auto", sheet = 1) {
  d <- suppressMessages(read_metadata_table(path, name, delimiter, sheet))
  names(d) <- trimws(names(d))
  if (ncol(d) < 2L || !nrow(d)) stop("Pathway files need a matching column, at least one annotation column, and at least one row.")
  if (anyNA(names(d)) || any(!nzchar(names(d))) || anyDuplicated(names(d)))
    stop("Pathway column names must be nonempty and unique after trimming spaces.")
  d[] <- lapply(d, as.character)
  d
}

pathway_auto_key <- function(table, features, data_type = "lipidomics") {
  preferred <- if (data_type == "metabolomics") c("Metabolite", "Lipid") else c("Lipid", "Metabolite")
  for (label in preferred) {
    at <- which(normalize_label(names(table)) == normalize_label(label))
    if (length(at)) return(names(table)[at[1L]])
  }
  ids <- unique(trimws(c(features$Lipid, features$Lipid_base)))
  overlap <- vapply(table, function(x) sum(ids %in% trimws(x)), integer(1))
  names(table)[which.max(overlap)]
}

match_pathways <- function(table, key, features, provenance = list()) {
  if (length(key) != 1L || is.na(key) || !key %in% names(table)) stop("Select a pathway matching column.")
  d <- table
  d[] <- lapply(d, function(x) trimws(as.character(x)))
  ids <- d[[key]]
  if (anyNA(ids) || any(ids %in% c("", "NA", "N/A"))) stop("The pathway matching column contains missing or blank keys.")
  d <- d[!duplicated(d), , drop = FALSE]
  conflicts <- unique(d[[key]][duplicated(d[[key]]) | duplicated(d[[key]], fromLast = TRUE)])
  if (length(conflicts)) stop("Conflicting pathway rows for matching key(s): ",
    paste(utils::head(conflicts, 10L), collapse = ", "), if (length(conflicts) > 10L) " (and more)" else "",
    ". Supply one consistent annotation row per key.")
  exact <- match(trimws(features$Lipid), d[[key]])
  base <- match(trimws(features$Lipid_base), d[[key]])
  at <- ifelse(is.na(exact), base, exact)
  status <- ifelse(!is.na(exact), "Exact", ifelse(!is.na(base), "Base name", "Unmatched"))
  fields <- setdiff(names(d), key)
  values <- d[at, fields, drop = FALSE]
  rownames(values) <- NULL
  audit <- data.frame(Lipid = features$Lipid, Lipid_base = features$Lipid_base,
    Pathway_key = d[[key]][at], Match_status = status, stringsAsFactors = FALSE)
  list(input_table = table, table = d, key = key, fields = fields, annotations = values,
    audit = audit, unused = d[!seq_len(nrow(d)) %in% at, , drop = FALSE],
    collapsed_rows = nrow(table) - nrow(d),
    provenance = c(provenance, list(matching_column = key,
      matching_rule = "Exact final ID, then recorded cleaned base name; case-sensitive; outer spaces trimmed")))
}

pathway_summary <- function(annotation) {
  if (is.null(annotation)) return(data.frame(Metric = "Pathway annotations", Value = "Not supplied"))
  data.frame(Metric = c("Retained features", "Exact matches", "Base-name matches", "Unmatched features",
      "Unused pathway rows", "Identical duplicate rows collapsed"),
    Value = c(nrow(annotation$audit), sum(annotation$audit$Match_status == "Exact"),
      sum(annotation$audit$Match_status == "Base name"), sum(annotation$audit$Match_status == "Unmatched"),
      nrow(annotation$unused), annotation$collapsed_rows), stringsAsFactors = FALSE)
}

pathway_default_field <- function(annotation, data_type = "lipidomics") {
  if (is.null(annotation)) return("")
  preferred <- if (data_type == "metabolomics") "Condensed_Path" else "Condensed_Class"
  if (preferred %in% annotation$fields) preferred else annotation$fields[1L]
}

pathway_field <- function(annotation, field, fallback = "") {
  choices <- c("", annotation$fields)
  if (length(field) == 1L && !is.na(field) && field %in% choices) field else
    if (fallback %in% choices) fallback else ""
}

pathway_choices <- function(annotation) {
  fields <- annotation$fields
  none <- "None"
  while (none %in% fields) none <- paste0(none, " (app)")
  stats::setNames(c("", fields), c(none, fields))
}

pathway_export_features <- function(table, annotation) {
  if (is.null(annotation)) return(table)
  at <- match(table$Lipid, annotation$audit$Lipid)
  extra <- annotation$annotations[at, , drop = FALSE]
  names(extra) <- paste0("Pathway: ", names(extra))
  rownames(extra) <- NULL
  cbind(table, extra)
}

pathway_tables <- function(annotation, context = NULL) {
  if (is.null(annotation)) return(list())
  list(pathway_input = annotation$input_table,
    pathway_feature_annotations = export_feature_table(pathway_export_features(annotation$audit, annotation),
      context, columns = c("Lipid", "Lipid_base")),
    pathway_unused_rows = annotation$unused,
    pathway_summary = pathway_summary(annotation),
    pathway_settings = data.frame(Setting = names(annotation$provenance),
      Value = vapply(annotation$provenance, as.character, character(1)), stringsAsFactors = FALSE))
}

pathway_spec <- function(annotation, field, mode = "categorical") {
  if (is.null(annotation) || identical(field, "")) return(NULL)
  # The synthetic ID header cannot collide with validated nonempty upload headers.
  metadata <- data.frame(annotation$audit$Lipid, annotation$annotations, check.names = FALSE)
  names(metadata)[1L] <- ""
  spec <- metadata_color_spec(metadata, field, mode)
  spec$field <- field
  if (spec$mode == "categorical") {
    lev <- levels(spec$values)
    spec$palette <- stats::setNames(grDevices::hcl.colors(max(1L, length(lev)), "Dark 3")[seq_along(lev)], lev)
  }
  spec
}

pathway_color_scale <- function(spec, aesthetic = "colour") {
  title <- paste("Feature:", spec$field)
  if (spec$mode == "continuous") {
    finite <- spec$values[is.finite(spec$values)]
    ggplot2::scale_colour_viridis_c(name = title, aesthetics = aesthetic,
      limits = if (length(finite)) range(finite) else NULL, na.value = "grey65",
      guide = ggplot2::guide_colourbar(available_aes = aesthetic))
  } else ggplot2::scale_colour_manual(name = title, aesthetics = aesthetic,
    values = spec$palette, limits = c(names(spec$palette), if (anyNA(spec$values)) NA_character_),
    na.value = "grey65", na.translate = TRUE, drop = FALSE,
    guide = ggplot2::guide_legend(ncol = if (length(spec$palette) > 12L) 1L else NULL))
}

# Long or numerous class labels fit beside the panel instead of spanning the page.
pathway_legend_theme <- function(spec) {
  if (is.null(spec) || spec$mode != "categorical" || length(spec$palette) <= 12L) return(NULL)
  ggplot2::theme(legend.position = "right", legend.direction = "vertical",
    legend.title = ggplot2::element_text(size = 9), legend.text = ggplot2::element_text(size = 8),
    legend.key.height = grid::unit(.36, "cm"), legend.key.width = grid::unit(.45, "cm"))
}

pathway_point_data <- function(table, annotation, color = "", mode = "categorical", shape = "") {
  at <- if (!is.null(annotation)) match(table$Lipid, annotation$audit$Lipid) else integer()
  color_spec <- pathway_spec(annotation, color, mode)
  shape_spec <- pathway_spec(annotation, shape, "categorical")
  if (!is.null(color_spec)) table$.pathway_color <- color_spec$values[at]
  if (!is.null(shape_spec)) {
    if (nlevels(shape_spec$values) > length(pca_shape_palette))
      stop("Feature shape mapping supports at most 12 categories. Choose a condensed category or None.")
    table$.pathway_shape <- shape_spec$values[at]
  }
  list(data = table, color = color_spec, shape = shape_spec)
}

pathway_point_layers <- function(points) {
  d <- points$data
  mapping <- if (!is.null(points$color) && !is.null(points$shape)) ggplot2::aes(colour = .pathway_color, shape = .pathway_shape) else
    if (!is.null(points$color)) ggplot2::aes(colour = .pathway_color) else ggplot2::aes(shape = .pathway_shape)
  args <- list(mapping = mapping, size = 2, alpha = .8)
  if (is.null(points$color)) args$colour <- "#176d80"
  layers <- list(do.call(ggplot2::geom_point, args))
  if (!is.null(points$color)) layers <- c(layers, list(pathway_color_scale(points$color)))
  if (!is.null(points$shape)) {
    lev <- levels(d$.pathway_shape)
    layers <- c(layers, list(ggplot2::scale_shape_manual(name = paste("Feature:", points$shape$field),
      values = stats::setNames(pca_shape_palette[seq_along(lev)], lev),
      limits = c(lev, if (anyNA(d$.pathway_shape)) NA_character_), na.value = 4, drop = FALSE)))
  }
  layers
}

# A separate aesthetic keeps feature arrows independent of sample colour scales.
# GeomSegment still supplies the coordinate handling, arrow drawing and legend key.
pathway_arrow_layer <- function(data) {
  geom <- ggplot2::ggproto("GeomPathwaySegment", ggplot2::GeomSegment,
    default_aes = utils::modifyList(ggplot2::GeomSegment$default_aes, ggplot2::aes(pathway_colour = "grey40")),
    draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                          lineend = "butt", linejoin = "round", na.rm = FALSE) {
      data$colour <- data$pathway_colour
      ggplot2::GeomSegment$draw_panel(data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin, na.rm = na.rm)
    },
    draw_key = function(data, params, size) {
      data$colour <- data$pathway_colour
      ggplot2::draw_key_path(data, params, size)
    })
  ggplot2::layer(geom = geom, stat = "identity", position = "identity", data = data,
    mapping = ggplot2::aes(x = 0, y = 0, xend = Displayed_x, yend = Displayed_y, pathway_colour = .pathway_color),
    inherit.aes = FALSE, show.legend = c(pathway_colour = TRUE, colour = FALSE, shape = FALSE), params = list(alpha = .8, arrow = grid::arrow(length = grid::unit(.15, "cm"))))
}

pathway_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(id = ns("panel"), open = "open",
    shiny::tags$summary("Optional pathway annotations — add after processing or loading prepared data"),
    shiny::uiOutput(ns("upload")), shiny::uiOutput(ns("read_controls")), shiny::uiOutput(ns("key_control")),
    shiny::uiOutput(ns("status")),
    shiny::tags$details(shiny::tags$summary("Preview and matching audit"),
      DT::DTOutput(ns("preview")), shiny::h5("Feature matches"), DT::DTOutput(ns("matches")),
      shiny::h5("Unused pathway rows"), DT::DTOutput(ns("unused"))))
}

pathway_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    generation <- shiny::reactiveVal(0L)
    # Run identity is independent of normalization, QC membership and plot controls.
    run_key <- shiny::reactive({
      r <- result()
      if (is.null(r)) return(NULL)
      list(run = r$pathway_run_id, completed = r$completed_at, signature = r$signature,
        features = pathway_features(r), type = analysis_terms(r)$id)
    })
    reset_upload <- function() generation(shiny::isolate(generation()) + 1L)
    context <- shiny::reactiveVal(NULL)
    shiny::observeEvent(run_key(), {
      key <- run_key()
      previous <- shiny::isolate(context())
      if (!identical(key, previous)) {
        if (!is.null(previous)) reset_upload()
        context(key)
      }
    }, ignoreNULL = FALSE, priority = 200)
    shiny::observeEvent(input$clear, reset_upload(), priority = 200)
    upload_id <- shiny::reactive(paste0("file_", generation()))
    upload <- shiny::reactive({
      if (is.null(context())) return(NULL)
      input[[upload_id()]]
    })
    output$upload <- shiny::renderUI({
      if (is.null(context())) return(shiny::p("Run processing or load prepared data first. A pathway file is optional."))
      shiny::tagList(shiny::fileInput(session$ns(upload_id()), "TCM / TBL pathway info (optional)",
        accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
        shiny::actionButton(session$ns("clear"), "Clear pathway annotations"),
        shiny::helpText("Match the file to final feature names. Recorded cleaned base names also match duplicate signals. Prepared data use supplied names. Categories change displays only; unmatched features remain included."))
    })
    sheets <- shiny::reactive({
      f <- upload()
      if (is.null(f) || !tolower(tools::file_ext(f$name)) %in% c("xlsx", "xls")) return(character())
      tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character())
    })
    output$read_controls <- shiny::renderUI({
      shiny::req(upload())
      if (length(sheets())) shiny::selectInput(session$ns("sheet"), "Pathway sheet", sheets()) else
        shiny::selectInput(session$ns("delimiter"), "Pathway delimiter", c("Automatic" = "auto", "Comma" = ",", "Tab" = "\t"))
    })
    imported <- shiny::reactive({
      f <- upload()
      if (is.null(f)) return(NULL)
      tryCatch({
        sheet <- if (length(sheets())) {
          if (length(input$sheet) == 1L && input$sheet %in% sheets()) input$sheet else sheets()[1L]
        } else 1
        delim <- if (is.null(input$delimiter)) "auto" else input$delimiter
        list(table = read_pathway_table(f$datapath, f$name, delim, sheet),
          provenance = list(filename = f$name, md5 = unname(tools::md5sum(f$datapath)),
            sheet = if (length(sheets())) as.character(sheet) else "not applicable", delimiter = delim))
      }, error = function(e) list(error = conditionMessage(e)))
    })
    default_key <- shiny::reactive({
      d <- imported()
      if (is.null(d) || !is.null(d$error)) return("")
      pathway_auto_key(d$table, context()$features, context()$type)
    })
    output$key_control <- shiny::renderUI({
      d <- imported()
      if (is.null(d) || !is.null(d$error)) return(NULL)
      columns <- names(d$table)
      shiny::selectInput(session$ns("key"), "Pathway matching column",
        choices = stats::setNames(columns, columns), selected = default_key())
    })
    state <- shiny::reactive({
      d <- imported()
      if (is.null(d) || !is.null(d$error)) return(d)
      key <- if (length(input$key) == 1L && input$key %in% names(d$table)) input$key else default_key()
      tryCatch(list(annotation = match_pathways(d$table, key, context()$features, d$provenance)),
        error = function(e) list(error = conditionMessage(e)))
    })
    annotation <- shiny::reactive(state()$annotation)
    output$status <- shiny::renderUI({
      s <- state()
      if (is.null(s)) return(NULL)
      if (!is.null(s$error)) return(shiny::div(class = "notice error", paste("Pathway annotations not applied:", s$error)))
      a <- s$annotation; counts <- pathway_summary(a)
      shiny::tagList(shiny::p(paste(paste(counts$Metric, counts$Value, sep = ": "), collapse = " | ")),
        if (all(a$audit$Match_status == "Unmatched")) shiny::div(class = "notice warn",
          "No features match. Check the matching column and final feature names; all features remain unannotated."))
    })
    widget <- function(d) DT::datatable(d, rownames = FALSE, escape = TRUE,
      options = list(pageLength = 5, scrollX = TRUE, lengthChange = FALSE))
    output$preview <- DT::renderDT({ shiny::req(imported()$table); widget(imported()$table) }, server = TRUE)
    output$matches <- DT::renderDT({ shiny::req(annotation()); widget(export_feature_table(pathway_export_features(annotation()$audit, annotation()), context()$type, columns = c("Lipid", "Lipid_base"))) }, server = TRUE)
    output$unused <- DT::renderDT({ shiny::req(annotation()); widget(annotation()$unused) }, server = TRUE)
    list(annotation = annotation, state = state, generation = generation)
  })
}
