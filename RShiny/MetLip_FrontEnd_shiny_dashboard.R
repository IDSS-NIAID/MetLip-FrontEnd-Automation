library(shiny)
library(shinydashboard)
library(jose)
library(httr)
library(jsonlite)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(readxl)
library(zip)

if (FALSE)
  devtools::install_github("IDSS-NIAID/MetLip-FrontEnd-Automation")
library(MetLipAutomation)


#### TAS API integration ####
# Credentials come from Posit Connect environment variables: TAS_PCS_API_KEY, TAS_PCS_UPN

tas_api_key <- Sys.getenv("TAS_PCS_API_KEY")
tas_upn     <- Sys.getenv("TAS_PCS_UPN")

tas_base_url      <- "https://rtb.nih.gov"
tas_gen_token_url <- paste0(tas_base_url, "/api/GenerateApiToken")
tas_download_url  <- paste0(tas_base_url, "/api/Pcs/DownloadSubmittedSamples")
tas_upload_url    <- paste0(tas_base_url, "/api/Pcs/UploadAcquiredSamples")

# Shared token cache (per R process). Refreshes when within 30s of expiry.
token_cache            <- new.env(parent = emptyenv())
token_cache$api_token  <- NULL
token_cache$expires_at <- 0L

# Build & sign the identity JWT for TAS
build_tas_identity_jwt <- function() {
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(
    sub = tas_upn,
    aud = "NiaidTasProduction",
    exp = now + 180L,
    nbf = now - 3L
  )
  jose::jwt_encode_hmac(claim, charToRaw(tas_api_key))
}

# Return cached TAS token or fetch a fresh one
get_tas_token <- function() {
  now <- as.integer(Sys.time())
  if (!is.null(token_cache$api_token) && now < token_cache$expires_at - 30L) {
    return(token_cache$api_token)
  }
  identity_jwt <- build_tas_identity_jwt()
  res <- httr::POST(
    url = tas_gen_token_url,
    httr::add_headers(Authorization = paste("Bearer", identity_jwt))
  )
  if (res$status_code != 200)
    stop(paste("GenerateApiToken failed — HTTP", res$status_code))
  token_cache$api_token  <- trimws(httr::content(res, "text", encoding = "UTF-8"))
  token_cache$expires_at <- now + 180L
  token_cache$api_token
}

# DownloadSubmittedSamples — body must be a raw JSON string (encode="json" fails)
tas_download <- function(request_id) {
  api_token <- get_tas_token()
  raw_body  <- paste0('{"RequestId":"', request_id, '"}')
  httr::POST(
    url  = tas_download_url,
    httr::add_headers(
      Authorization  = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ),
    body = raw_body
  )
}

# UploadAcquiredSamples — payload is a raw JSON string
tas_upload <- function(upload_payload) {
  api_token <- get_tas_token()
  httr::POST(
    url  = tas_upload_url,
    httr::add_headers(
      Authorization  = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ),
    body = upload_payload
  )
}

# detect submitted sample ID column
detect_sub_id_col <- function(df) {
  intersect(
    c("Submitted Sample Ids", "SubmittedSampleIds", "SubmittedSampleId", "SampleId"),
    colnames(df)
  )[1]
}

#### User Interface ####
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "50px",
               style = "margin-right: 10px; vertical-align: left;"),
      "Comp Tools for Mass Spec"
    ),
    titleWidth = 400
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TAS Accelerator", tabName = "acquired_tab", icon = icon("cogs")),
      menuItem("Sequence Generator",  tabName = "plate_tab",    icon = icon("th"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
      .main-header .logo,
      .main-header .navbar          { background-color: #157b8d; }
      .main-header .logo:hover      { background-color: #126d7d; }
      .main-sidebar                 { background-color: #0f5e6d; }
      .sidebar-menu > li > a        { color: #d6f0f4; }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover  { background-color: #157b8d;
                                      color: #ffffff;
                                      border-left-color: #a8dde5; }
      .sidebar-menu > li > a .fa    { color: #a8dde5; }
      .method-badge {
        display: inline-flex; align-items: center;
        background-color: #157b8d; color: white;
        border-radius: 12px; padding: 4px 10px;
        margin: 4px; font-size: 13px; font-weight: bold;
      }
      .method-badge .remove-btn {
        background: none; border: none; color: white;
        margin-left: 6px; cursor: pointer;
        font-size: 14px; padding: 0; line-height: 1;
      }
      .method-badge .remove-btn:hover { color: #ffcccc; }
      #ms_methods_ui { min-height: 36px; margin-top: 8px; }
      .status-msg               { margin-top: 10px; font-weight: bold; font-size: 14px; }
      .status-msg.success       { color: #00a65a; }
      .status-msg.error         { color: #dd4b39; }
      .selection-active         { color: #157b8d; font-weight: bold; font-size: 15px;
                                  margin-top: 8px; margin-bottom: 8px; }
      .selection-idle           { color: #6c757d; font-weight: bold; font-size: 15px;
                                  margin-top: 8px; margin-bottom: 8px; }
    ")),

    tabItems(

      # TAB 1: TAS Accelerator
      tabItem(tabName = "acquired_tab",

        fluidRow(box(
          title = "Technology Accessioning System (TAS) Search Request", width = 12,

          p("Enter a Project Request ID to retrieve submitted samples from TAS."),
          fluidRow(
            column(6, textInput("request_id_input", "Project Request ID:",
                                placeholder = "e.g. yeboahr2-20260108-ML1")),
            column(2, br(), actionButton("search_btn", "Search",
                                         icon = icon("search"),
                                         class = "btn-primary btn-block")),
            column(4, br(), uiOutput("search_status"))
          )
        )),

        shinyjs::hidden(div(id = "ms_builder_section",
          fluidRow(box(
            title = "Generate Acquired Sample Names", width = 12,
            p("Enter one MS Method at a time."),
            fluidRow(
              column(4, textInput("ms_input", "Enter MS Method:",
                                  placeholder = "type here")),
              column(2, br(), actionButton("add_method_btn", "Add Method",
                                           icon = icon("plus"), class = "btn-success")),
              column(3, br(), actionButton("generate_btn", "Generate",
                                           icon = icon("table"), class = "btn-warning")),
              column(3, br(), actionButton("clear_methods_btn", "Clear Methods",
                                           icon = icon("times"), class = "btn-default"))
            ),
            div(strong("Added Methods:"), uiOutput("ms_methods_ui"))
          ))
        )),

        shinyjs::hidden(div(id = "table_section",
          fluidRow(box(
            title = uiOutput("table_title"), width = 12,
            DTOutput("samples_table")
          ))
        )),

        shinyjs::hidden(div(id = "upload_section",
          fluidRow(box(
            title = "Upload to TAS", width = 12,
            fluidRow(
              column(3, actionButton("upload_btn", "Send to TAS",
                                     icon = icon("cloud-upload"),
                                     class = "btn-success btn-block")),
              column(3, downloadButton("download_btn", "Download Excel",
                                       icon = icon("download"),
                                       class = "btn-primary btn-block")),
              column(6, uiOutput("upload_status"))
            )
          ))
        ))
      ),

      # TAB 2: Sequence Generator
      tabItem(tabName = "plate_tab",

        # Data source
        fluidRow(box(
          title = "Data Source", width = 12,
          radioButtons("data_source", "Choose data source:",
                       choices = c("Use acquired samples directly from the TAS Accelerator tab" = "tas",
                                   "Upload Excel file" = "upload"),
                       selected = "tas", inline = TRUE),
          shinyjs::hidden(div(id = "upload_panel",
            fileInput("plate_file_input", "Upload Acquired Samples Excel:",
                      accept = c(".xlsx", ".xls")),
            tags$p(tags$strong("Note:"),
                   " keep the ", tags$code("Notes"),
                   " column in your parsed file — it is required for plate generation.",
                   style = "color: #dd4b39; font-size: 13px; margin-top: -8px;")
          )),
          uiOutput("plate_project_id_ui"),
          br(),
          actionButton("load_source_btn", "Load Data",
                       icon = icon("upload"), class = "btn-primary"),
          br(), br(),
          uiOutput("source_status")
        )),

        # Reference table: acquired samples
        shinyjs::hidden(div(id = "reference_section",
          fluidRow(box(
            title = "Acquired Samples", width = 12,
            p("When assigning multiple sample types",
              "click the first and last samples of each range."),
            DTOutput("reference_table")
          ))
        )),

        # Matrix assignment
        shinyjs::hidden(div(id = "matrix_section",
          fluidRow(box(
            title = "Sample Matrix Assignment", width = 12,

            checkboxInput("multi_matrix",
                          "This project contains multiple sample types",
                          value = FALSE),

            shinyjs::hidden(div(id = "multi_matrix_panel",
              p("Type a matrix, click ", strong("Select Sample Range"),
                ", then click the first and last samples in the table above."),
              fluidRow(
                column(3, textInput("new_matrix", "Matrix Name:",
                                    placeholder = "type here")),
                column(3, br(), actionButton("reset_selection_btn", "Select Sample Range",
                                             class = "btn-info btn-block")),
                column(3, textInput("new_first_id", "First Submitted Sample ID:",
                                    placeholder = "click a row")),
                column(3, textInput("new_last_id", "Last Submitted Sample ID:",
                                    placeholder = "click a row"))
              ),
              uiOutput("selection_status"),
              fluidRow(
                column(3, actionButton("add_matrix_btn", "Add Range",
                                       icon = icon("plus"),
                                       class = "btn-success btn-block"))
              ),
              br(),
              uiOutput("matrix_assignments_ui"),
              br()
            )),

            shinyjs::hidden(div(id = "single_matrix_panel",
              textInput("single_matrix_name", "Matrix Name:",
                        placeholder = "type here")
            )),

            br(),
            actionButton("validate_btn", "Validate & Preview",
                         icon = icon("check"), class = "btn-primary")
          ))
        )),

        # Validated preview
        shinyjs::hidden(div(id = "validation_section",
          fluidRow(box(
            title = "Validated Data Preview", width = 12,
            uiOutput("validation_status"),
            DTOutput("validated_table"),
            br(),
            h4("Randomization Grouping"),
            p("Select column(s) to randomize within. The order of groups is",
              "shuffled, and samples are shuffled within each group.",
              "Leave all unchecked for a simple shuffle of all samples."),
            uiOutput("group_cols_ui"),
            br(),
            actionButton("generate_plate_btn", "Generate Plate Meta Data",
                         icon = icon("th"), class = "btn-warning")
          ))
        )),

        # Plate data + sequence generation
        shinyjs::hidden(div(id = "plate_section",
          fluidRow(box(
            title = "Plate Loading Data", width = 12,
            selectInput("plate_matrix_select", "View Matrix:", choices = NULL),
            DTOutput("plate_table"),
            br(),
            h4("LC Methods"),
            p("Enter the LC Method for each MS Method present in this project."),
            uiOutput("lc_inputs_ui"),
            br(),
            h4("QC & Blank Positions"),
            p("Positions for the QC and blank vials."),
            fluidRow(
              column(3, numericInput("qc_plate", "QC Plate:",
                                     value = 3, min = 1, step = 1)),
              column(3, numericInput("qc_vial", "QC Vial Position:",
                                     value = 53, min = 1, step = 1)),
              column(3, numericInput("blank_plate", "Blank Plate:",
                                     value = 3, min = 1, step = 1)),
              column(3, numericInput("blank_vial", "Blank Vial Position:",
                                     value = 54, min = 1, step = 1))
            ),
            fluidRow(
              column(4, numericInput("qc_blank_interval",
                                     "Insert QC and Blank every N samples:",
                                     value = 10, min = 1, step = 1))
            ),
            br(),
            fluidRow(
              column(4, numericInput("injection_vol", "Injection Volume (µL):",
                                     value = 5, min = 2, max = 20, step = 0.5)),
              column(4, br(), actionButton("generate_seq_btn", "Generate Sequence",
                                           icon = icon("coffee"), class = "btn-warning")),
              column(4, br(), uiOutput("sequence_status"))
            )
          ))
        )),

        # Download
        shinyjs::hidden(div(id = "download_section",
          fluidRow(box(
            title = "Download", width = 12,
            p("Downloads a ZIP containing plate loading data (CSV and Excel)",
              "and sequence files (CSV)."),
            downloadButton("download_all_zip", "Download",
                           icon = icon("download"), class = "btn-primary")
          ))
        ))
      )
    )
  )
)

#### Server ####
server <- function(input, output, session) {

  rv <- reactiveValues(
    df_submitted     = NULL,
    df_generated     = NULL,
    df_confirmed     = NULL,
    request_id       = NULL,
    ms_methods       = character(),
    id_col           = NULL,
    confirm_bytes    = NULL,
    confirm_filename = NULL,
    df_source        = NULL,
    plate_id_col     = NULL,
    parsed_cols      = character(),
    df_validated     = NULL,
    plate_results    = NULL,
    ms_methods_detected = character(),
    seq_dir          = NULL,
    seq_paths        = NULL
  )

  matrix_assignments <- reactiveVal(
    data.frame(Matrix = character(0), FirstID = character(0),
               LastID = character(0), stringsAsFactors = FALSE)
  )

  selection_mode <- reactiveVal("idle")

  # TAB 1: Acquired Samples

  observeEvent(input$search_btn, {
    raw_id <- trimws(input$request_id_input)
    if (nchar(raw_id) == 0) {
      showNotification("Enter a Request Display ID.", type = "warning"); return()
    }
    output$search_status <- renderUI(tags$p("Searching...", class = "status-msg"))

    res <- tryCatch(
      tas_download(raw_id),
      error = function(e) {
        showNotification(paste("Cannot reach TAS:", e$message, "Call Ian"), type = "error"); NULL
      }
    )
    if (is.null(res)) { output$search_status <- renderUI(NULL); return() }

    if (res$status_code == 200) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(content(res, "raw"), temp_file)
      df <- tryCatch(readxl::read_excel(temp_file), error = function(e) {
        showNotification("Cannot read the file.", type = "error"); NULL
      })
      if (is.null(df)) { output$search_status <- renderUI(NULL); return() }

      id_col <- detect_sub_id_col(df)
      if (is.na(id_col)) {
        showNotification("Cannot find Submitted Sample ID column.", type = "error")
        output$search_status <- renderUI(NULL); return()
      }

      rv$df_submitted  <- df
      rv$df_generated  <- NULL
      rv$df_confirmed  <- NULL
      rv$request_id    <- raw_id
      rv$ms_methods    <- character()
      rv$id_col        <- id_col
      rv$confirm_bytes <- NULL
      rv$confirm_filename <- NULL

      shinyjs::show("ms_builder_section")
      shinyjs::show("table_section")
      shinyjs::hide("upload_section")
      output$search_status <- renderUI(
        tags$p(paste0(nrow(df), " submitted sample(s) loaded."),
               class = "status-msg success")
      )
      output$upload_status <- renderUI(NULL)

    } else {
      err <- tryCatch({
        detail <- content(res, "text", encoding = "UTF-8")
        if (!is.null(detail) && nchar(detail) > 0)
          paste0("HTTP ", res$status_code, " — ", detail)
        else paste("HTTP", res$status_code)
      }, error = function(e) paste("HTTP", res$status_code))
      output$search_status <- renderUI(
        tags$p(paste("Whoopsie", err), class = "status-msg error")
      )
    }
  })

  output$table_title <- renderUI({
    if (!is.null(rv$df_confirmed))     "Confirmed Acquired Samples"
    else if (!is.null(rv$df_generated)) "Generated Acquired Sample Names"
    else                                "Submitted Samples"
  })

  output$samples_table <- renderDT({
    df <- if (!is.null(rv$df_confirmed))     rv$df_confirmed
          else if (!is.null(rv$df_generated)) rv$df_generated
          else                                rv$df_submitted
    req(df)
    datatable(df, options = list(scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$add_method_btn, {
    method <- trimws(input$ms_input)
    if (nchar(method) == 0) {
      showNotification("Please enter an MS Method name.", type = "warning"); return()
    }
    if (method %in% rv$ms_methods) {
      showNotification(paste(method, "already added."), type = "warning"); return()
    }
    rv$ms_methods <- c(rv$ms_methods, method)
    updateTextInput(session, "ms_input", value = "")
  })

  observeEvent(input$clear_methods_btn, {
    rv$ms_methods   <- character()
    rv$df_generated <- NULL
    shinyjs::hide("upload_section")
  })

  output$ms_methods_ui <- renderUI({
    methods <- rv$ms_methods
    if (length(methods) == 0) return(tags$em("No methods added yet."))
    lapply(seq_along(methods), function(i) {
      tags$span(class = "method-badge", methods[i],
        tags$button(class = "remove-btn",
          onclick = paste0("Shiny.setInputValue('remove_method', ", i,
                           ", {priority: 'event'})"), "✕"))
    })
  })

  observeEvent(input$remove_method, {
    idx <- input$remove_method
    if (!is.null(idx) && idx >= 1 && idx <= length(rv$ms_methods))
      rv$ms_methods <- rv$ms_methods[-idx]
  })

  observeEvent(input$generate_btn, {
    req(rv$df_submitted)
    if (length(rv$ms_methods) == 0) {
      showNotification("Add at least one MS Method first.", type = "warning"); return()
    }
    id_col <- rv$id_col
    expanded <- lapply(rv$ms_methods, function(m) {
      rv$df_submitted %>%
        mutate(`Acquired Sample Name` = paste(.data[[id_col]], m, sep = "_"))
    }) %>% bind_rows()

    col_order <- c("Acquired Sample Name",
                   setdiff(colnames(expanded), "Acquired Sample Name"))
    rv$df_generated <- expanded %>% select(all_of(col_order))

    shinyjs::show("upload_section")
    showNotification(
      paste0("Generated ", nrow(rv$df_generated), " acquired sample name(s)."),
      type = "message", duration = 4
    )
  })

  observeEvent(input$upload_btn, {
    req(rv$df_generated, rv$request_id)
    shinyjs::disable("upload_btn")
    output$upload_status <- renderUI(tags$p("Uploading...", class = "status-msg"))

    df     <- rv$df_generated
    id_col <- rv$id_col

    samples_json <- paste(
      sapply(seq_len(nrow(df)), function(i) {
        paste0(
          '{"AcquiredSampleName":"', df[["Acquired Sample Name"]][i], '",',
          '"SubmittedSampleIds":["', df[[id_col]][i], '"],',
          '"Notes":"', ifelse(is.na(df[["Notes"]][i]), "", df[["Notes"]][i]), '"}'
        )
      }),
      collapse = ","
    )
    upload_payload <- paste0(
      '{"RequestDisplayId":"', rv$request_id, '",',
      '"AcquiredSamples":[', samples_json, ']}'
    )

    res <- tryCatch(
      tas_upload(upload_payload),
      error = function(e) {
        output$upload_status <- renderUI(
          tags$p(paste("TAS unreachable:", e$message, ". Call Ian"), class = "status-msg error"))
        shinyjs::enable("upload_btn"); NULL
      }
    )
    if (is.null(res)) return()

    if (res$status_code == 200) {
      raw_bytes    <- content(res, "raw")
      content_disp <- res$headers[["content-disposition"]]
      filename     <- regmatches(content_disp,
                                 regexpr('filename="([^"]+)"', content_disp))
      filename     <- gsub('filename="|"', "", filename)
      if (length(filename) == 0 || nchar(filename) == 0)
        filename <- paste0(rv$request_id, "_acquired_samples.xlsx")

      rv$confirm_bytes    <- raw_bytes
      rv$confirm_filename <- filename

      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(raw_bytes, temp_file)
      df_confirmed <- tryCatch(readxl::read_excel(temp_file), error = function(e) NULL)
      if (!is.null(df_confirmed)) {
        colnames(df_confirmed)[colnames(df_confirmed) == "Acquired Sample ID (blank if new)"] <-
          "Acquired Sample ID"
        rv$df_confirmed <- df_confirmed
      }
      output$upload_status <- renderUI(
        tags$p("TAS upload succesful, go to next dashboard tab",
               class = "status-msg success")
      )
    } else {
      err <- tryCatch({
        detail <- content(res, "text", encoding = "UTF-8")
        if (!is.null(detail) && nchar(detail) > 0)
          paste0("HTTP ", res$status_code, " — ", detail)
        else paste("HTTP", res$status_code)
      }, error = function(e) paste("HTTP", res$status_code))
      output$upload_status <- renderUI(
        tags$p(paste("❌ Upload failed:", err), class = "status-msg error")
      )
    }
    shinyjs::enable("upload_btn")
  })

  output$download_btn <- downloadHandler(
    filename = function() {
      if (!is.null(rv$confirm_filename)) rv$confirm_filename
      else paste0(rv$request_id, "_acquired_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      if (!is.null(rv$confirm_bytes)) writeBin(rv$confirm_bytes, file)
      else write_xlsx(rv$df_generated, file)
    }
  )

  # TAB 2: Plate & Sequence

  observeEvent(input$data_source, {
    if (input$data_source == "upload") shinyjs::show("upload_panel")
    else                               shinyjs::hide("upload_panel")
  })

  output$plate_project_id_ui <- renderUI({
    req(input$data_source)
    if (input$data_source == "upload") {
      textInput("plate_project_id", "Project ID:",
                placeholder = "e.g. yeboahr2-20260108-ML1")
    } else if (input$data_source == "tas" && !is.null(rv$request_id)) {
      tagList(
        tags$label("Project ID:"),
        tags$p(rv$request_id,
               style = "font-weight: bold; font-size: 14px; margin-top: 4px;")
      )
    } else {
      NULL
    }
  })

  observeEvent(input$load_source_btn, {
    if (input$data_source == "tas") {
      if (is.null(rv$df_confirmed)) {
        showNotification(
          "No TAS data available. Please complete the Acquired Samples workflow first.",
          type = "warning"); return()
      }
      rv$df_source <- rv$df_confirmed
      output$source_status <- renderUI(
        tags$p(paste0("Loaded ", nrow(rv$df_confirmed),
                      " rows from TAS Acquired Samples tab."),
               class = "status-msg success")
      )
    } else {
      req(input$plate_file_input)
      df <- tryCatch(readxl::read_excel(input$plate_file_input$datapath),
                     error = function(e) {
                       showNotification("Could not read uploaded file.", type = "error"); NULL
                     })
      if (is.null(df)) return()
      rv$df_source <- df
      output$source_status <- renderUI(
        tags$p(paste0("Loaded ", nrow(df), " rows from uploaded file."),
               class = "status-msg success")
      )
    }

    rv$plate_id_col <- detect_sub_id_col(rv$df_source)

    # Detect parsed/extra columns beyond the standard TAS columns
    standard_cols <- c("Acquired Sample ID", "Acquired Sample Name",
                       "Submitted Sample Ids", "Submitted Sample Names", "Notes")
    rv$parsed_cols <- setdiff(colnames(rv$df_source), standard_cols)

    rv$df_validated  <- NULL
    rv$plate_results <- NULL
    rv$seq_dir       <- NULL
    rv$seq_paths     <- NULL
    selection_mode("idle")
    matrix_assignments(data.frame(Matrix = character(0), FirstID = character(0),
                                  LastID = character(0), stringsAsFactors = FALSE))

    shinyjs::show("reference_section")
    shinyjs::show("matrix_section")
    shinyjs::hide("validation_section")
    shinyjs::hide("plate_section")
    shinyjs::hide("download_section")
  })

  output$reference_table <- renderDT({
    req(rv$df_source)
    datatable(rv$df_source,
              selection = "single",
              options = list(scrollX = TRUE),
              rownames = FALSE)
  })

  observeEvent(input$multi_matrix, {
    if (isTRUE(input$multi_matrix)) {
      shinyjs::show("multi_matrix_panel")
      shinyjs::hide("single_matrix_panel")
    } else {
      shinyjs::hide("multi_matrix_panel")
      shinyjs::show("single_matrix_panel")
      selection_mode("idle")
    }
  })

  observe({
    shinyjs::disable("new_first_id")
    shinyjs::disable("new_last_id")
  })

  observeEvent(input$reset_selection_btn, {
    selection_mode("first")
    updateTextInput(session, "new_first_id", value = "")
    updateTextInput(session, "new_last_id",  value = "")
  })

  output$selection_status <- renderUI({
    mode <- selection_mode()
    if (mode == "first") {
      tags$p("Click the FIRST sample in the range (table above).",
             class = "selection-active")
    } else if (mode == "last") {
      tags$p("Click the LAST sample in the range (table above).",
             class = "selection-active")
    } else {
      tags$p("Click 'Select Sample Range' to begin, then click + Add Range.",
             class = "selection-idle")
    }
  })

  observeEvent(input$reference_table_rows_selected, {
    idx <- input$reference_table_rows_selected
    if (is.null(idx)) return()
    req(rv$df_source, rv$plate_id_col)

    sel_id <- rv$df_source[[rv$plate_id_col]][idx]
    mode   <- selection_mode()

    if (mode == "first") {
      updateTextInput(session, "new_first_id", value = sel_id)
      selection_mode("last")
    } else if (mode == "last") {
      updateTextInput(session, "new_last_id", value = sel_id)
      selection_mode("idle")
    }
  })

  observeEvent(input$add_matrix_btn, {
    mat   <- trimws(input$new_matrix)
    first <- trimws(input$new_first_id)
    last  <- trimws(input$new_last_id)
    if (any(c(mat, first, last) == "")) {
      showNotification("Enter a matrix and select both first and last samples.",
                       type = "warning")
      return()
    }
    matrix_assignments(rbind(
      matrix_assignments(),
      data.frame(Matrix = mat, FirstID = first, LastID = last, stringsAsFactors = FALSE)
    ))
    updateTextInput(session, "new_matrix",   value = "")
    updateTextInput(session, "new_first_id", value = "")
    updateTextInput(session, "new_last_id",  value = "")
    selection_mode("idle")
  })

  observeEvent(input$remove_matrix, {
    idx <- input$remove_matrix
    ma  <- matrix_assignments()
    if (!is.null(idx) && idx >= 1 && idx <= nrow(ma))
      matrix_assignments(ma[-idx, , drop = FALSE])
  })

  output$matrix_assignments_ui <- renderUI({
    ma <- matrix_assignments()
    if (nrow(ma) == 0) return(tags$em("No matrix added yet."))
    tags$table(class = "table table-bordered table-condensed",
      style = "max-width: 700px;",
      tags$thead(tags$tr(
        tags$th("Matrix"), tags$th("First Submitted Sample ID"),
        tags$th("Last Submitted Sample ID"), tags$th("")
      )),
      tags$tbody(lapply(seq_len(nrow(ma)), function(i) {
        tags$tr(
          tags$td(ma$Matrix[i]), tags$td(ma$FirstID[i]), tags$td(ma$LastID[i]),
          tags$td(tags$button("✕", class = "btn btn-danger btn-xs",
            onclick = paste0("Shiny.setInputValue('remove_matrix', ", i,
                             ", {priority: 'event'})")))
        )
      }))
    )
  })

  observeEvent(input$validate_btn, {
    req(rv$df_source)

    project_id <- if (!is.null(input$data_source) && input$data_source == "tas" &&
                      !is.null(rv$request_id)) {
      rv$request_id
    } else {
      trimws(input$plate_project_id)
    }
    if (nchar(project_id) == 0) {
      showNotification("Enter a Project ID.", type = "warning"); return()
    }

    df         <- rv$df_source
    sub_id_col <- rv$plate_id_col
    if (is.na(sub_id_col)) {
      showNotification("Cannot find Submitted Sample ID column.", type = "error"); return()
    }

    if (isTRUE(input$multi_matrix)) {
      ma <- matrix_assignments()
      if (nrow(ma) == 0) {
        showNotification("Add at least one matrix.", type = "warning")
        return()
      }
      unique_ids <- unique(df[[sub_id_col]])
      df$Matrix  <- NA_character_
      for (i in seq_len(nrow(ma))) {
        first_pos <- which(unique_ids == ma$FirstID[i])
        last_pos  <- which(unique_ids == ma$LastID[i])
        if (length(first_pos) == 0 || length(last_pos) == 0) {
          showNotification(paste("Could not find IDs for matrix:", ma$Matrix[i]),
                           type = "warning"); next
        }
        ids_in_range <- unique_ids[first_pos:last_pos]
        df$Matrix[df[[sub_id_col]] %in% ids_in_range] <- ma$Matrix[i]
      }
      n_unassigned <- sum(is.na(df$Matrix))
      if (n_unassigned > 0)
        showNotification(paste(n_unassigned, "rows have no matrix assigned. Double check ranges."),
                         type = "warning")
    } else {
      mat_name <- trimws(input$single_matrix_name)
      if (nchar(mat_name) == 0) {
        showNotification("Enter a sample type.", type = "warning"); return()
      }
      df$Matrix    <- mat_name
      n_unassigned <- 0L
    }

    if ("Acquired Sample Name" %in% colnames(df))
      df$MS_method <- sub(".*_", "", df[["Acquired Sample Name"]])

    df <- df %>% mutate(
      Project_ID         = project_id,
      Acquired_Sample_ID = `Acquired Sample ID`,
      Date_Processed     = Sys.Date()
    )

    rv$df_validated <- df

    preview_cols <- intersect(
      c("Project_ID", sub_id_col, "Submitted Sample Names",
        "Acquired Sample Name", "Matrix", "MS_method",
        "Acquired_Sample_ID", "Notes"),
      colnames(df)
    )

    output$validation_status <- renderUI(
      tags$p(paste0("✅ ", nrow(df), " rows validated. ", n_unassigned, " unassigned."),
             class = "status-msg success")
    )
    output$validated_table <- renderDT(
      datatable(df %>% select(all_of(preview_cols)),
                options = list(scrollX = TRUE), rownames = FALSE)
    )
    shinyjs::show("validation_section")
    shinyjs::hide("plate_section")
    shinyjs::hide("download_section")
  })

  # Render randomization grouping checkboxes from detected parsed columns
  output$group_cols_ui <- renderUI({
    cols <- rv$parsed_cols
    if (length(cols) == 0)
      return(tags$em("No extra columns detected, the default randomization will be used."))
    checkboxGroupInput("group_by_cols", label = NULL,
                       choices = cols, inline = TRUE)
  })

  observeEvent(input$generate_plate_btn, {
    req(rv$df_validated)

    plate_meta <- tryCatch(
      MetLipAutomation::generate_plate_meta_data(rv$df_validated, randomize = FALSE),
      error = function(e) {
        showNotification(paste("Plate generation failed:", e$message), type = "error"); NULL
      }
    )
    if (is.null(plate_meta)) return()

    processed <- tryCatch(
      MetLipAutomation::process_plate_data(plate_meta, rv$df_validated),
      error = function(e) {
        showNotification(paste("Plate processing failed:", e$message), type = "error"); NULL
      }
    )
    if (is.null(processed)) return()

    # Join parsed columns onto plate_loading by Submitted Sample ID, so they
    # appear in the plate meta data files and are available for randomization
    # grouping inside generate_sequence(). Metadata repeats across MS methods
    # by design, so joining on Submitted Sample ID alone is sufficient.
    if (length(rv$parsed_cols) > 0) {
      sub_col <- rv$plate_id_col
      parsed_lookup <- rv$df_source %>%
        select(all_of(c(sub_col, rv$parsed_cols))) %>%
        distinct()
      join_by_vec <- setNames(sub_col, "Submitted_Sample_ID")
      processed$plate_loading <- processed$plate_loading %>%
        left_join(parsed_lookup, by = join_by_vec)
    }

    rv$plate_results <- processed
    rv$seq_dir       <- NULL
    rv$seq_paths     <- NULL

    matrices <- unique(processed$plate_loading$Matrix)
    matrices <- matrices[!is.na(matrices)]
    updateSelectInput(session, "plate_matrix_select", choices = matrices)

    ms_present <- unique(processed$plate_loading$MS_method)
    ms_present <- ms_present[!is.na(ms_present)]
    rv$ms_methods_detected <- ms_present

    shinyjs::show("plate_section")
    shinyjs::hide("download_section")
    showNotification("Plate meta data generated.", type = "message")
  })

  output$lc_inputs_ui <- renderUI({
    ms <- rv$ms_methods_detected
    if (length(ms) == 0) return(tags$em("No MS methods detected."))
    lapply(seq_along(ms), function(i) {
      fluidRow(column(6,
        textInput(paste0("lc_method_", i),
                  label = paste0("LC Method for ", ms[i], ":"),
                  placeholder = "type here")
      ))
    })
  })

  output$plate_table <- renderDT({
    req(rv$plate_results, input$plate_matrix_select)
    df <- rv$plate_results$plate_loading %>%
      filter(Matrix == input$plate_matrix_select)
    datatable(df, options = list(scrollX = TRUE), rownames = FALSE)
  })

  # Generate Sequence
  observeEvent(input$generate_seq_btn, {
    req(rv$plate_results)
    output$sequence_status <- renderUI(
      tags$span(style = "color:#6c757d; font-weight:600;",
                icon("hourglass-half"), " make some coffee...")
    )

    ms <- rv$ms_methods_detected
    lc_vals <- sapply(seq_along(ms), function(i) {
      val <- input[[paste0("lc_method_", i)]]
      if (is.null(val)) "" else trimws(val)
    })
    lc_map <- setNames(lc_vals, ms)

    if (any(lc_vals == "")) {
      showNotification("One or more LC Methods are missing.",
                       type = "warning", duration = 6)
    }

    td <- tempfile("seq_")
    dir.create(td, recursive = TRUE)

    paths <- tryCatch(
      MetLipAutomation::generate_sequence(
        rv$plate_results$plate_loading,
        rv$plate_results$qc_plate_data,
        rv$plate_results$blank_plate_data,
        rv$plate_results$project_id,
        injection_vol = input$injection_vol,
        lc_methods    = lc_map,
        qc_blank_interval       = input$qc_blank_interval,
        group_by_cols           = input$group_by_cols,
        qc_plate_override       = input$qc_plate,
        qc_position_override    = input$qc_vial,
        blank_plate_override    = input$blank_plate,
        blank_position_override = input$blank_vial,
        output_dir    = td
      ),
      error = function(e) {
        showNotification(paste("Sequence generation failed:", e$message), type = "error"); NULL
      }
    )
    if (is.null(paths) || length(paths) == 0) return()

    rv$seq_dir   <- td
    rv$seq_paths <- paths

    output$sequence_status <- renderUI(
      tags$span(class = "text-success", style = "font-weight:600;",
                icon("check-circle"),
                paste(length(paths), "sequence file(s) ready."))
    )
    shinyjs::show("download_section")
    showNotification(paste("Generated", length(paths), "sequence file(s)."),
                     type = "message")
  })

  output$download_all_zip <- downloadHandler(
    filename = function() {
      pid <- rv$plate_results$project_id
      paste0("Sequence_", pid, "_", format(Sys.Date(), "%Y%m%d"), ".zip")
    },
    content = function(file) {
      req(rv$plate_results, rv$seq_paths, rv$seq_dir)

      td <- tempfile("download_")
      dir.create(file.path(td, "plate_data"), recursive = TRUE)
      dir.create(file.path(td, "sequences"),  recursive = TRUE)

      matrices <- unique(rv$plate_results$plate_loading$Matrix)
      matrices <- matrices[!is.na(matrices)]
      for (mat in matrices) {
        mat_df <- rv$plate_results$plate_loading %>% filter(Matrix == mat)
        write.csv(mat_df,
                  file.path(td, "plate_data", paste0(mat, "_plate_data.csv")),
                  row.names = FALSE)
        writexl::write_xlsx(mat_df,
                  file.path(td, "plate_data", paste0(mat, "_plate_data.xlsx")))
      }

      for (p in rv$seq_paths)
        file.copy(p, file.path(td, "sequences", basename(p)))

      tmpzip <- tempfile(fileext = ".zip")
      zip::zipr(zipfile = tmpzip,
                files = list.files(td, recursive = TRUE, full.names = FALSE),
                root = td)
      file.copy(tmpzip, file, overwrite = TRUE)
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
