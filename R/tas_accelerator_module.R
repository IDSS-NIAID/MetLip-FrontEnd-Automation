#' tas_accelerator_ui
#' UI element for TAS eaccelerator tab
#' 
#' @param id NS session id
#' 
#' @return A shiny UI element
#' @importFrom shiny NS
#' @importFrom shinyjs hidden
tas_accelerator_ui <- function(id) {
  ns <- shiny::NS(id)
      tabItem(tabName = "acquired_tab",
              
              fluidRow(box(
                title = "Technology Accessioning System (TAS) Search Request", width = 12,
                
                p("Enter a Project Request ID to retrieve submitted samples from TAS."),
                fluidRow(
                  column(6, textInput(ns("request_id_input"), "Project Request ID:",
                                      placeholder = "e.g. yeboahr2-20260108-ML1")),
                  column(2, br(), actionButton(ns("search_btn"), "Search",
                                               icon = icon("search"),
                                               class = "btn-primary btn-block")),
                  column(4, br(), uiOutput(ns("search_status")))
                )
              )),
              
              shinyjs::hidden(div(id = ns("ms_builder_section"),
                                  fluidRow(box(
                                    title = "Generate Acquired Sample Names", width = 12,
                                    p("Enter one MS Method at a time."),
                                    fluidRow(
                                      column(4, textInput(ns("ms_input"), "Enter MS Method:",
                                                          placeholder = "type here")),
                                      column(2, br(), actionButton(ns("add_method_btn"), "Add Method",
                                                                   icon = icon("plus"), class = "btn-success")),
                                      column(3, br(), actionButton(ns("generate_btn"), "Generate",
                                                                   icon = icon("table"), class = "btn-warning")),
                                      column(3, br(), actionButton(ns("clear_methods_btn"), "Clear Methods",
                                                                   icon = icon("times"), class = "btn-default"))
                                    ),
                                    div(strong("Added Methods:"), uiOutput(ns("ms_methods_ui")))
                                  ))
              )),
              
              shinyjs::hidden(div(id = ns("table_section"),
                                  fluidRow(box(
                                    title = uiOutput(ns("table_title")), width = 12,
                                    DTOutput(ns("samples_table"))
                                  ))
              )),
              
              shinyjs::hidden(div(id = ns("upload_section"),
                                  fluidRow(box(
                                    title = "Upload to TAS", width = 12,
                                    fluidRow(
                                      column(3, actionButton(ns("upload_btn"), "Send to TAS",
                                                             icon = icon("cloud-upload"),
                                                             class = "btn-success btn-block")),
                                      column(3, downloadButton(ns("download_btn"), "Download Excel",
                                                               icon = icon("download"),
                                                               class = "btn-primary btn-block")),
                                      column(6, uiOutput(ns("upload_status")))
                                    )
                                  ))
              ))
      )
}

#' tas_accelerator_server
#' Server function for TAS eaccelerator tab
#' 
#' @param id NS session id
#' @param app_state current app state
#' @param tas_client TAS client connector
#' 
#' @importFrom readxl read_excel
#' @importFrom shiny moduleServer reactiveValues updateTextInput
tas_accelerator_server <- function(id, app_state, tas_client = MetLipAutomation::new_tas_client()) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      df_submitted = NULL, df_generated = NULL, df_confirmed = NULL,
      request_id = NULL, ms_methods = character(), id_col = NULL,
      confirm_bytes = NULL, confirm_filename = NULL
    )
  # TAB 1: Acquired Samples
  
  observeEvent(input$search_btn, {
    raw_id <- trimws(input$request_id_input)
    if (nchar(raw_id) == 0) {
      showNotification("Enter a Request Display ID.", type = "warning"); return()
    }
    output$search_status <- renderUI(tags$p("Searching...", class = "status-msg"))
    
    res <- tryCatch(
      MetLipAutomation::tas_download_submitted_samples(tas_client, raw_id),
      error = function(e) {
        showNotification(paste("Cannot reach TAS:", e$message, "Call Ian"), type = "error"); NULL
      }
    )
    if (is.null(res)) { output$search_status <- renderUI(NULL); return() }
    
    if (res$status_code == 200) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(httr::content(res, "raw"), temp_file)
      df <- tryCatch(readxl::read_excel(temp_file), error = function(e) {
        showNotification("Cannot read the file.", type = "error"); NULL
      })
      if (is.null(df)) { output$search_status <- renderUI(NULL); return() }
      
      id_col <- MetLipAutomation::detect_submitted_sample_id_column(df)
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
      app_state$request_id <- raw_id
      app_state$df_submitted <- df
      app_state$df_generated <- NULL
      app_state$df_confirmed <- NULL
      app_state$submitted_id_col <- id_col
      
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
        detail <- httr::content(res, "text", encoding = "UTF-8")
        if (!is.null(detail) && nchar(detail) > 0)
          paste0("HTTP ", res$status_code, " \u0336 ", detail)
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
                            onclick = paste0("Shiny.setInputValue('", session$ns("remove_method"), "', ", i,
                                             ", {priority: 'event'})"), "\u2715"))
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
    
    
    res <- tryCatch(
      MetLipAutomation::tas_upload_acquired_samples(tas_client, rv$request_id, df, id_col),
      error = function(e) {
        output$upload_status <- renderUI(
          tags$p(paste("TAS unreachable:", e$message, ". Call Ian"), class = "status-msg error"))
        shinyjs::enable("upload_btn"); NULL
      }
    )
    if (is.null(res)) return()
    
    if (res$status_code == 200) {
      raw_bytes    <- httr::content(res, "raw")
      content_disp <- res$headers[["content-disposition"]]
      filename     <- regmatches(content_disp,
                                 regexpr('filename="([^"]+)"', content_disp))
      filename     <- gsub('filename="|"', "", filename)
      if (length(filename) == 0 || nchar(filename) == 0)
        filename <- paste0(rv$request_id, "_acquired_samples.xlsx")
      
      rv$confirm_bytes    <- raw_bytes
      rv$confirm_filename <- filename
      app_state$df_confirmed <- NULL
      app_state$request_id <- rv$request_id
      app_state$df_submitted <- rv$df_submitted
      app_state$df_generated <- rv$df_generated
      app_state$submitted_id_col <- rv$id_col
      
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(raw_bytes, temp_file)
      df_confirmed <- tryCatch(readxl::read_excel(temp_file), error = function(e) NULL)
      if (!is.null(df_confirmed)) {
        colnames(df_confirmed)[colnames(df_confirmed) == "Acquired Sample ID (blank if new)"] <-
          "Acquired Sample ID"
        rv$df_confirmed <- df_confirmed
        app_state$df_confirmed <- df_confirmed
      }
      output$upload_status <- renderUI(
        tags$p("TAS upload succesful, go to next dashboard tab",
               class = "status-msg success")
      )
    } else {
      err <- tryCatch({
        detail <- httr::content(res, "text", encoding = "UTF-8")
        if (!is.null(detail) && nchar(detail) > 0)
          paste0("HTTP ", res$status_code, " \u0336 ", detail)
        else paste("HTTP", res$status_code)
      }, error = function(e) paste("HTTP", res$status_code))
      output$upload_status <- renderUI(
        tags$p(paste("\u2715 Upload failed:", err), class = "status-msg error")
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
  

  })
}
