library(shiny)
library(shinydashboard)
library(jose)
library(httr)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(readxl)

# Local shared secret (Shiny and Plumber) 
my_secret <- charToRaw(Sys.getenv("SECRET_KEY"))

# Function: build a local JWT for talking to Plumber
make_local_jwt <- function(request_id = "") {
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(
    sub = "shiny-user",
    aud = "plumber-api",
    iss = "tas-accelerator-app",
    exp = now + 300L,
    id  = request_id
  )
  jose::jwt_encode_hmac(claim, my_secret)
}

# User Interface
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$img(
        src    = "logo.png",
        height = "38px",
        style  = "margin-right: 8px; vertical-align: middle;"
      ),
      "TAS Accelerator"
    ),
    titleWidth = 280
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Acquired Samples", tabName = "acquired_tab", icon = icon("flask"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    
    tags$style(HTML("
      .main-header .logo {
        background-color: #157b8d !important;
      }
      .main-header .navbar {
        background-color: #157b8d !important;
      }
      .main-header .logo:hover {
        background-color: #126d7d !important;
      }
      .main-sidebar {
        background-color: #0f5e6d !important;
      }
      .sidebar-menu > li > a {
        color: #d6f0f4 !important;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover {
        background-color: #157b8d !important;
        color: #ffffff !important;
        border-left-color: #a8dde5 !important;
      }
      .sidebar-menu > li > a .fa {
        color: #a8dde5 !important;
      }
      .method-badge {
        display: inline-flex; align-items: center;
        background-color: #3c8dbc; color: white;
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
      .status-msg { margin-top: 10px; font-weight: bold; font-size: 14px; }
      .status-msg.success { color: #00a65a; }
      .status-msg.error   { color: #dd4b39; }
    ")),
    
    tabItems(
      tabItem(tabName = "acquired_tab",
              
              # ── Search ─────────────────────────────────────────────────────────────
              fluidRow(
                box(
                  title = "Search Request", width = 12,
                  p("Enter a Request Display ID to retrieve submitted samples from TAS.",
                    "Authentication is handled automatically."),
                  fluidRow(
                    column(6,
                           textInput("request_id_input", "Request Display ID:",
                                     placeholder = "e.g. yeboahr2-20260108-ML1")
                    ),
                    column(2, br(),
                           actionButton("search_btn", "Search",
                                        icon = icon("search"), class = "btn-primary btn-block")
                    ),
                    column(4, br(),
                           uiOutput("search_status")
                    )
                  )
                )
              ),
              
              # ── MS Method builder ──────────────────────────────────────────────────
              shinyjs::hidden(div(id = "ms_builder_section",
                                  fluidRow(
                                    box(
                                      title = "Generate Acquired Sample Names", width = 12,
                                      fluidRow(
                                        column(4,
                                               textInput("ms_input", "Enter MS Method:",
                                                         placeholder = "e.g. TCMU")
                                        ),
                                        column(2, br(),
                                               actionButton("add_method_btn", "Add Method",
                                                            icon = icon("plus"), class = "btn-success")
                                        ),
                                        column(3, br(),
                                               actionButton("generate_btn", "Generate",
                                                            icon = icon("table"), class = "btn-warning")
                                        ),
                                        column(3, br(),
                                               actionButton("clear_methods_btn", "Clear Methods",
                                                            icon = icon("times"), class = "btn-default")
                                        )
                                      ),
                                      div(strong("Added Methods:"), uiOutput("ms_methods_ui"))
                                    )
                                  )
              )),
              
              # ── Results table ──────────────────────────────────────────────────────
              shinyjs::hidden(div(id = "table_section",
                                  fluidRow(
                                    box(
                                      title = uiOutput("table_title"), width = 12,
                                      DTOutput("samples_table")
                                    )
                                  )
              )),
              
              # ── Upload ─────────────────────────────────────────────────────────────
              shinyjs::hidden(div(id = "upload_section",
                                  fluidRow(
                                    box(
                                      title = "Upload to TAS", width = 12,
                                      fluidRow(
                                        column(3,
                                               actionButton("upload_btn", "Send to TAS",
                                                            icon = icon("cloud-upload"),
                                                            class = "btn-success btn-block")
                                        ),
                                        column(3,
                                               downloadButton("download_btn", "Download Excel",
                                                              icon = icon("download"),
                                                              class = "btn-primary btn-block")
                                        ),
                                        column(6, uiOutput("upload_status"))
                                      )
                                    )
                                  )
              ))
              
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    df_submitted   = NULL,   # raw submitted samples from TAS
    df_generated   = NULL,   # table after MS method generation
    df_confirmed   = NULL,   # confirmation table returned by TAS after upload
    request_id     = NULL,
    ms_methods     = character(),
    id_col         = NULL,   # detected submitted sample ID column name
    confirm_bytes  = NULL,   # raw bytes of confirmation xlsx
    confirm_filename = NULL  # filename from Content-Disposition header
  )
  
  # ── Search: download submitted samples from TAS ────────────────────────────
  observeEvent(input$search_btn, {
    raw_id <- trimws(input$request_id_input)
    if (nchar(raw_id) == 0) {
      showNotification("Please enter a Request Display ID.", type = "warning")
      return()
    }
    
    output$search_status <- renderUI(
      tags$p("⏳ Searching...", class = "status-msg")
    )
    
    jwt <- make_local_jwt(raw_id)
    
    res <- tryCatch(
      POST("http://localhost:8000/pcs/download",
           add_headers(Authorization = paste("Bearer", jwt))),
      error = function(e) {
        showNotification(paste("Could not reach Plumber:", e$message), type = "error")
        NULL
      }
    )
    if (is.null(res)) {
      output$search_status <- renderUI(NULL)
      return()
    }
    
    if (res$status_code == 200) {
      # Save and read the returned xlsx
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(content(res, "raw"), temp_file)
      
      df <- tryCatch(
        readxl::read_excel(temp_file),
        error = function(e) {
          showNotification("Could not read returned Excel file.", type = "error")
          NULL
        }
      )
      if (is.null(df)) {
        output$search_status <- renderUI(NULL)
        return()
      }
      
      # Detect submitted sample ID column
      id_col <- intersect(
        c("Submitted Sample Ids", "SubmittedSampleIds", "SubmittedSampleId", "SampleId"),
        colnames(df)
      )[1]
      
      if (is.na(id_col)) {
        showNotification("Cannot find a Submitted Sample ID column in returned data.",
                         type = "error")
        output$search_status <- renderUI(NULL)
        return()
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
        tags$p(paste0("✅ ", nrow(df), " submitted sample(s) loaded."),
               class = "status-msg success")
      )
      output$upload_status <- renderUI(NULL)
      
    } else {
      err <- tryCatch({
        p <- content(res, "parsed")
        detail <- if (!is.null(p$details)) paste0(" — ", p$details) else ""
        if (!is.null(p$error)) paste0(p$error, detail) else paste("HTTP", res$status_code)
      }, error = function(e) paste("HTTP", res$status_code))
      output$search_status <- renderUI(
        tags$p(paste("❌", err), class = "status-msg error")
      )
    }
  })
  
  # ── Table title ────────────────────────────────────────────────────────────
  output$table_title <- renderUI({
    if (!is.null(rv$df_confirmed)) {
      "Confirmed Acquired Samples"
    } else if (!is.null(rv$df_generated)) {
      "Generated Acquired Sample Names"
    } else {
      "Submitted Samples"
    }
  })
  
  # ── Table display ──────────────────────────────────────────────────────────
  output$samples_table <- renderDT({
    df <- if (!is.null(rv$df_confirmed)) {
      rv$df_confirmed
    } else if (!is.null(rv$df_generated)) {
      rv$df_generated
    } else {
      rv$df_submitted
    }
    req(df)
    datatable(df, options = list(scrollX = TRUE), rownames = FALSE)
  })
  
  # ── Add MS Method ──────────────────────────────────────────────────────────
  observeEvent(input$add_method_btn, {
    method <- trimws(input$ms_input)
    if (nchar(method) == 0) {
      showNotification("Please enter an MS Method name.", type = "warning")
      return()
    }
    if (method %in% rv$ms_methods) {
      showNotification(paste(method, "already added."), type = "warning")
      return()
    }
    rv$ms_methods <- c(rv$ms_methods, method)
    updateTextInput(session, "ms_input", value = "")
  })
  
  # ── Clear Methods ──────────────────────────────────────────────────────────
  observeEvent(input$clear_methods_btn, {
    rv$ms_methods  <- character()
    rv$df_generated <- NULL
    shinyjs::hide("upload_section")
  })
  
  # ── Method badges ──────────────────────────────────────────────────────────
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
  
  # ── Generate Acquired Sample Names ─────────────────────────────────────────
  observeEvent(input$generate_btn, {
    req(rv$df_submitted)
    if (length(rv$ms_methods) == 0) {
      showNotification("Add at least one MS Method first.", type = "warning")
      return()
    }
    
    id_col <- rv$id_col
    
    # One row per submitted sample × MS method
    expanded <- lapply(rv$ms_methods, function(m) {
      rv$df_submitted %>%
        mutate(`Acquired Sample Name` = paste(.data[[id_col]], m, sep = "_"))
    }) %>% bind_rows()
    
    # Column order: Acquired Sample Name first, then rest of original columns
    col_order <- c("Acquired Sample Name",
                   setdiff(colnames(expanded), "Acquired Sample Name"))
    rv$df_generated <- expanded %>% select(all_of(col_order))
    
    shinyjs::show("upload_section")
    showNotification(
      paste0("Generated ", nrow(rv$df_generated), " acquired sample name(s)."),
      type = "message", duration = 4
    )
  })
  
  # ── Download Excel (pre-upload) ────────────────────────────────────────────
  output$download_btn <- downloadHandler(
    filename = function()
      paste0(rv$request_id, "_acquired_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) write_xlsx(rv$df_generated, file)
  )
  
  # ── Upload to TAS ──────────────────────────────────────────────────────────
  observeEvent(input$upload_btn, {
    req(rv$df_generated, rv$request_id)
    shinyjs::disable("upload_btn")
    output$upload_status <- renderUI(
      tags$p("⏳ Uploading...", class = "status-msg")
    )
    
    df   <- rv$df_generated
    id_col <- rv$id_col
    
    # Build raw JSON payload row by row
    samples_json <- paste(
      sapply(seq_len(nrow(df)), function(i) {
        paste0(
          '{"AcquiredSampleName":"', df[["Acquired Sample Name"]][i], '",',
          '"SubmittedSampleIds":["', df[[id_col]][i], '"],',
          '"Notes":"',
          ifelse(is.na(df[["Notes"]][i]), "", df[["Notes"]][i]),
          '"}'
        )
      }),
      collapse = ","
    )
    
    upload_payload <- paste0(
      '{"RequestDisplayId":"', rv$request_id, '",',
      '"AcquiredSamples":[', samples_json, ']}'
    )
    
    jwt <- make_local_jwt(rv$request_id)
    
    res <- tryCatch(
      POST("http://localhost:8000/pcs/upload",
           add_headers(Authorization  = paste("Bearer", jwt),
                       `Content-Type` = "application/json"),
           body   = charToRaw(upload_payload),
           encode = "raw"),
      error = function(e) {
        output$upload_status <- renderUI(
          tags$p(paste("❌ API unreachable:", e$message), class = "status-msg error"))
        shinyjs::enable("upload_btn")
        NULL
      }
    )
    if (is.null(res)) return()
    
    if (res$status_code == 200) {
      raw_bytes <- content(res, "raw")
      
      # Extract filename from Content-Disposition header
      content_disp <- res$headers[["content-disposition"]]
      filename <- regmatches(content_disp,
                             regexpr('filename="([^"]+)"', content_disp))
      filename <- gsub('filename="|"', "", filename)
      if (length(filename) == 0 || nchar(filename) == 0)
        filename <- paste0(rv$request_id, "_acquired_samples.xlsx")
      
      rv$confirm_bytes    <- raw_bytes
      rv$confirm_filename <- filename
      
      # Read confirmation xlsx and repopulate the table
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(raw_bytes, temp_file)
      df_confirmed <- tryCatch(
        readxl::read_excel(temp_file),
        error = function(e) {
          showNotification("Could not read confirmation file.", type = "warning")
          NULL
        }
      )
      if (!is.null(df_confirmed)) {
        colnames(df_confirmed)[colnames(df_confirmed) == "Acquired Sample ID (blank if new)"] <- "Acquired Sample ID"
        rv$df_confirmed <- df_confirmed
      }
      
      output$upload_status <- renderUI(
        tags$p(paste0("✅ Upload successful. Download confirmation below."),
               class = "status-msg success")
      )
      
    } else {
      err <- tryCatch({
        p <- content(res, "parsed")
        detail <- if (!is.null(p$details)) paste0(" — ", p$details) else ""
        if (!is.null(p$error)) paste0(p$error, detail) else paste("HTTP", res$status_code)
      }, error = function(e) paste("HTTP", res$status_code))
      output$upload_status <- renderUI(
        tags$p(paste("❌ Upload failed:", err), class = "status-msg error")
      )
    }
    shinyjs::enable("upload_btn")
  })
  
  # ── Download confirmation xlsx ─────────────────────────────────────────────
  output$download_btn <- downloadHandler(
    filename = function() {
      if (!is.null(rv$confirm_filename)) rv$confirm_filename
      else paste0(rv$request_id, "_acquired_",
                  format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      if (!is.null(rv$confirm_bytes)) {
        writeBin(rv$confirm_bytes, file)
      } else {
        write_xlsx(rv$df_generated, file)
      }
    }
  )
  
}

shinyApp(ui, server)