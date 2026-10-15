sequence_generator_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "plate_tab",
              
              # Data source
              fluidRow(box(
                title = "Data Source", width = 12,
                radioButtons(ns("data_source"), "Choose data source:",
                             choices = c("Use acquired samples directly from the TAS Accelerator tab" = "tas",
                                         "Upload Excel file" = "upload"),
                             selected = "tas", inline = TRUE),
                shinyjs::hidden(div(id = ns("upload_panel"),
                                    fileInput(ns("plate_file_input"), "Upload Acquired Samples Excel:",
                                              accept = c(".xlsx", ".xls")),
                                    tags$p(tags$strong("Note:"),
                                           " if the Acquired Sample ID column header contains ",
                                           tags$code("(blank if new)"),
                                           ", it will be removed automatically during upload.",
                                           style = "color: #dd4b39; font-size: 13px; margin-top: -8px;"),
                                    tags$p(tags$strong("Note:"),
                                           " keep the ", tags$code("Notes"),
                                           " column in your parsed file. The column is required for plate generation.",
                                           style = "color: #dd4b39; font-size: 13px; margin-top: -8px;")
                )),
                uiOutput(ns("plate_project_id_ui")),
                br(),
                actionButton(ns("load_source_btn"), "Load Data",
                             icon = icon("upload"), class = "btn-primary"),
                br(), br(),
                uiOutput(ns("source_status"))
              )),
              
              # Reference table: acquired samples
              shinyjs::hidden(div(id = ns("reference_section"),
                                  fluidRow(box(
                                    title = "Acquired Samples", width = 12,
                                    p("When assigning multiple sample types",
                                      "click the first and last samples of each range."),
                                    DTOutput(ns("reference_table"))
                                  ))
              )),
              
              # Matrix assignment
              shinyjs::hidden(div(id = ns("matrix_section"),
                                  fluidRow(box(
                                    title = "Sample Matrix Assignment", width = 12,
                                    
                                    checkboxInput(ns("multi_matrix"),
                                                  "This project contains multiple sample types",
                                                  value = FALSE),
                                    
                                    shinyjs::hidden(div(id = ns("multi_matrix_panel"),
                                                        p("Type a matrix, click ", strong("Select Sample Range"),
                                                          ", then click the first and last samples in the table above."),
                                                        fluidRow(
                                                          column(3, textInput(ns("new_matrix"), "Matrix Name:",
                                                                              placeholder = "type here")),
                                                          column(3, br(), actionButton(ns("reset_selection_btn"), "Select Sample Range",
                                                                                       class = "btn-info btn-block")),
                                                          column(3, textInput(ns("new_first_id"), "First Submitted Sample ID:",
                                                                              placeholder = "click a row")),
                                                          column(3, textInput(ns("new_last_id"), "Last Submitted Sample ID:",
                                                                              placeholder = "click a row"))
                                                        ),
                                                        uiOutput(ns("selection_status")),
                                                        fluidRow(
                                                          column(3, actionButton(ns("add_matrix_btn"), "Add Range",
                                                                                 icon = icon("plus"),
                                                                                 class = "btn-success btn-block"))
                                                        ),
                                                        br(),
                                                        uiOutput(ns("matrix_assignments_ui")),
                                                        br()
                                    )),
                                    
                                    shinyjs::hidden(div(id = ns("single_matrix_panel"),
                                                        textInput(ns("single_matrix_name"), "Matrix Name:",
                                                                  placeholder = "type here")
                                    )),
                                    
                                    br(),
                                    actionButton(ns("validate_btn"), "Validate & Preview",
                                                  class = "btn-primary")
                                  ))
              )),
              
              # Validated preview
              shinyjs::hidden(div(id = ns("validation_section"),
                                  fluidRow(box(
                                    title = "Validated Data Preview", width = 12,
                                    uiOutput(ns("validation_status")),
                                    DTOutput(ns("validated_table")),
                                    br(),
                                    h4("Randomization Grouping"),
                                    checkboxInput(ns("no_randomize"),
                                                  "No Randomization",
                                                  value = FALSE),
                                    div(id = ns("grouping_controls"),
                                        p("Select column(s) to randomize within.",
                                          "Leave all unchecked for default randomization of all samples per plate."),
                                        uiOutput(ns("group_cols_ui"))
                                    ),
                                    
                                    # Level ordering (draggable tiles)
                                    br(),
                                    h4("Group Level Order (optional)"),
                                    p("For each checked variable below, drag the level tiles into the",
                                      "order you want groups to run. Groups run in that fixed order and",
                                      "samples are randomized within each group.",
                                      strong("Save Order"), " to apply. Any variable you don't save keeps",
                                      "random group order."),
                                    uiOutput(ns("level_order_ui")),
                                    uiOutput(ns("level_order_status")),
                                    
                                    
                                    br(),
                                    actionButton(ns("generate_plate_btn"), "Generate Plate Meta Data",
                                                 icon = icon("th"), class = "btn-warning")
                                  ))
              )),
              
              # Plate data + sequence generation
              shinyjs::hidden(div(id = ns("plate_section"),
                                  fluidRow(box(
                                    title = "Plate Loading Data", width = 12,
                                    selectInput(ns("plate_matrix_select"), "View Matrix:", choices = NULL),
                                    DTOutput(ns("plate_table")),
                                    br(),
                                    h4("LC and MS methods formated for SciexOS"),
                                    p("Enter the specific LC Method",
                                      "and the MS Method names."),
                                    uiOutput(ns("lc_inputs_ui")),
                                    br(),
                                    h4("QC & Blank Positions"),
                                    p("Positions for the QC and blank vials."),
                                    fluidRow(
                                      column(3, numericInput(ns("qc_plate"), "QC Plate:",
                                                             value = 3, min = 1, step = 1)),
                                      column(3, numericInput(ns("qc_vial"), "QC Vial Position:",
                                                             value = 53, min = 1, step = 1)),
                                      column(3, numericInput(ns("blank_plate"), "Blank Plate:",
                                                             value = 3, min = 1, step = 1)),
                                      column(3, numericInput(ns("blank_vial"), "Blank Vial Position:",
                                                             value = 54, min = 1, step = 1))
                                    ),
                                    fluidRow(
                                      column(4, numericInput(ns("qc_blank_interval"),
                                                             "Insert QC and Blank every N samples:",
                                                             value = 10, min = 1, step = 1))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(4, numericInput(ns("injection_vol"), "Injection Volume (µL):",
                                                             value = 5, min = 2, max = 20, step = 0.5)),
                                      column(4, br(), actionButton(ns("generate_seq_btn"), "Generate Sequence",
                                                                   icon = icon("coffee"), class = "btn-warning")),
                                      column(4, br(), uiOutput(ns("sequence_status")))
                                    )
                                  ))
              )),
              
              # Download
              shinyjs::hidden(div(id = ns("download_section"),
                                  fluidRow(box(
                                    title = "Download", width = 12,
                                    p("Downloads a ZIP containing plate loading data (CSV and Excel)",
                                      "and sequence files (CSV)."),
                                    downloadButton(ns("download_all_zip"), "Download",
                                                   icon = icon("download"), class = "btn-primary")
                                  ))
              ))
      )
}

sequence_generator_server <- function(id, tas_samples, detect_sub_id_col) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      df_source = NULL,
      plate_id_col = NULL,
      parsed_cols = character(),
      df_validated = NULL,
      plate_results = NULL,
      ms_methods_detected = character(),
      seq_dir = NULL,
      seq_paths = NULL,
      level_orders = list()
    )

    matrix_assignments <- shiny::reactiveVal(
      data.frame(Matrix = character(0), FirstID = character(0),
                 LastID = character(0), stringsAsFactors = FALSE)
    )
    selection_mode <- shiny::reactiveVal("idle")

  observeEvent(input$data_source, {
    if (input$data_source == "upload") shinyjs::show(session$ns("upload_panel"))
    else                               shinyjs::hide(session$ns("upload_panel"))
  })
  
  output$plate_project_id_ui <- renderUI({
    req(input$data_source)
    if (input$data_source == "upload") {
      textInput(session$ns("plate_project_id"), "Project ID:",
                placeholder = "e.g. yeboahr2-20260108-ML1")
    } else if (input$data_source == "tas" && !is.null(tas_samples()$request_id)) {
      tagList(
        tags$label("Project ID:"),
        tags$p(tas_samples()$request_id,
               style = "font-weight: bold; font-size: 14px; margin-top: 4px;")
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$load_source_btn, {
    if (input$data_source == "tas") {
      if (is.null(tas_samples()$df_confirmed)) {
        showNotification(
          "No TAS data available. Please complete the Acquired Samples workflow first.",
          type = "warning"); return()
      }
      rv$df_source <- tas_samples()$df_confirmed
      output$source_status <- renderUI(
        tags$p(paste0("Loaded ", nrow(tas_samples()$df_confirmed),
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
      
      # Strip a "(blank if new)" suffix from any column header 
      cleaned <- gsub("\\s*\\(blank if new\\)\\s*$", "",
                      colnames(df), ignore.case = TRUE)
      if (!identical(cleaned, colnames(df))) {
        colnames(df) <- cleaned
        showNotification("Removed '(blank if new)' from column header(s).",
                         type = "message", duration = 4)
      }
      
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
    rv$level_orders  <- list()
    selection_mode("idle")
    matrix_assignments(data.frame(Matrix = character(0), FirstID = character(0),
                                  LastID = character(0), stringsAsFactors = FALSE))
    
    shinyjs::show(session$ns("reference_section"))
    shinyjs::show(session$ns("matrix_section"))
    shinyjs::hide(session$ns("validation_section"))
    shinyjs::hide(session$ns("plate_section"))
    shinyjs::hide(session$ns("download_section"))
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
      shinyjs::show(session$ns("multi_matrix_panel"))
      shinyjs::hide(session$ns("single_matrix_panel"))
    } else {
      shinyjs::hide(session$ns("multi_matrix_panel"))
      shinyjs::show(session$ns("single_matrix_panel"))
      selection_mode("idle")
    }
  })
  
  observe({
    shinyjs::disable(session$ns("new_first_id"))
    shinyjs::disable(session$ns("new_last_id"))
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
                                       onclick = paste0("Shiny.setInputValue('", session$ns("remove_matrix"), "', ", i,
                                                        ", {priority: 'event'})")))
                 )
               }))
    )
  })
  
  observeEvent(input$validate_btn, {
    req(rv$df_source)
    
    project_id <- if (!is.null(input$data_source) && input$data_source == "tas" &&
                      !is.null(tas_samples()$request_id)) {
      tas_samples()$request_id
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
    rv$level_orders <- list()   # reset any prior saved orders on re-validate
    
    preview_cols <- intersect(
      c("Project_ID", sub_id_col, "Submitted Sample Names",
        "Acquired Sample Name", "Matrix", "MS_method",
        "Acquired_Sample_ID", "Notes"),
      colnames(df)
    )
    
    output$validation_status <- renderUI(
      tags$p(paste0(" ", nrow(df), " rows validated. ", n_unassigned, " unassigned."),
             class = "status-msg success")
    )
    output$validated_table <- renderDT(
      datatable(df %>% select(all_of(preview_cols)),
                options = list(scrollX = TRUE), rownames = FALSE)
    )
    shinyjs::show(session$ns("validation_section"))
    shinyjs::hide(session$ns("plate_section"))
    shinyjs::hide(session$ns("download_section"))
  })
  
  
  # Hide grouping + level-order controls when No Randomization is selected
  observeEvent(input$no_randomize, {
    if (isTRUE(input$no_randomize)) {
      shinyjs::hide(session$ns("grouping_controls"))
      shinyjs::hide(session$ns("level_order_ui"))
      shinyjs::hide(session$ns("level_order_status"))
    } else {
      shinyjs::show(session$ns("grouping_controls"))
      shinyjs::show(session$ns("level_order_ui"))
      shinyjs::show(session$ns("level_order_status"))
    }
  }, ignoreInit = TRUE)
  
  # Render randomization grouping checkboxes from detected parsed columns
  output$group_cols_ui <- renderUI({
    cols <- rv$parsed_cols
    if (length(cols) == 0)
      return(tags$em("No extra columns detected, the default randomization will be used."))
    checkboxGroupInput(session$ns("group_by_cols"), label = NULL,
                       choices = cols, inline = TRUE)
  })
  
  # Level ordering
  # Render one rank_list of draggable level tiles per checked grouping variable,
  # plus a single shared Save Order button. Levels are read from the validated
  # data. If a variable already has a saved order, seed the tiles with it.
  output$level_order_ui <- renderUI({
    checked <- input$group_by_cols
    if (is.null(checked) || length(checked) == 0)
      return(tags$em("Check one or more variables above to order their levels."))
    req(rv$df_validated)
    
    tiles <- lapply(checked, function(gc) {
      if (!gc %in% colnames(rv$df_validated)) return(NULL)
      present_levels <- unique(as.character(rv$df_validated[[gc]]))
      present_levels <- present_levels[!is.na(present_levels)]
      if (length(present_levels) == 0) return(NULL)
      
      # Seed order: saved order (filtered to present) then any new levels,
      # otherwise the natural data order.
      saved <- rv$level_orders[[gc]]
      if (!is.null(saved) && length(saved) > 0) {
        seed <- c(saved[saved %in% present_levels],
                  setdiff(present_levels, saved))
      } else {
        seed <- present_levels
      }
      
      tagList(
        div(class = "rank-var-label", gc),
        sortable::rank_list(
          text      = NULL,
          labels    = seed,
          input_id  = session$ns(paste0("levord_", gc)),
          class     = "rank-list-container"
        )
      )
    })
    tiles <- tiles[!vapply(tiles, is.null, logical(1))]
    if (length(tiles) == 0)
      return(tags$em("No orderable levels found for the checked variable(s)."))
    
    tagList(
      tiles,
      br(),
      actionButton(session$ns("save_level_order_btn"), "Save Order",
                   icon = icon("save"), class = "btn-info")
    )
  })
  
  # Save button: read every checked variable's current tile order into
  # rv$level_orders. Only checked variables are saved; unchecked are dropped.
  observeEvent(input$save_level_order_btn, {
    checked <- input$group_by_cols
    if (is.null(checked) || length(checked) == 0) {
      showNotification("Check at least one variable before saving an order.",
                       type = "warning"); return()
    }
    new_orders <- list()
    for (gc in checked) {
      ord <- input[[paste0("levord_", gc)]]
      if (!is.null(ord) && length(ord) > 0)
        new_orders[[gc]] <- as.character(ord)
    }
    rv$level_orders <- new_orders
    showNotification(
      paste0("Saved level order for: ", paste(names(new_orders), collapse = ", ")),
      type = "message", duration = 4)
  })
  
  # If the checkbox selection changes, any previously saved order may no longer
  # match. Clear saved orders so the user re-saves against the current selection.
  observeEvent(input$group_by_cols, {
    if (length(rv$level_orders) > 0) {
      rv$level_orders <- list()
      showNotification(
        "Grouping selection changed. Re-drag and Save Order if you want a fixed order.",
        type = "warning", duration = 5)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  output$level_order_status <- renderUI({
    if (length(rv$level_orders) == 0)
      return(NULL)
    lines <- vapply(names(rv$level_orders), function(gc)
      paste0(gc, ": ", paste(rv$level_orders[[gc]], collapse = ", ")),
      character(1))
    tags$div(class = "order-saved-msg",
             tags$span("Saved order:"),
             lapply(lines, function(l) tags$div(l)))
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
    
    
    # Join parsed columns onto plate_loading so they are available for
    # randomization grouping inside generate_sequence().
    if (length(rv$parsed_cols) > 0) {
      sub_col <- rv$plate_id_col
      
      # One row per submitted sample id. If covariates vary within an id the
      # grouping is ambiguous, so warn rather than silently fan out.
      parsed_lookup <- rv$df_source %>%
        select(all_of(c(sub_col, rv$parsed_cols))) %>%
        distinct()
      
      dup_ids <- parsed_lookup %>%
        count(.data[[sub_col]]) %>%
        filter(n > 1)
      if (nrow(dup_ids) > 0)
        showNotification(
          paste("Warning: covariates are not unique per sample for",
                nrow(dup_ids), "ID(s). Grouping may be unreliable."),
          type = "warning", duration = 10)
      
      # Resolve the join key from plate_loading's ACTUAL column names, trying the
      # common spellings that process_plate_data() might use.
      candidate_keys <- c("Submitted_Sample_ID", "Submitted Sample Ids",
                          "SubmittedSampleIds", "Submitted_Sample_Ids",
                          "Submitted Sample ID", sub_col)
      target_key <- candidate_keys[candidate_keys %in% colnames(processed$plate_loading)][1]
      
      if (is.na(target_key)) {
        showNotification(
          paste0("Could not find a Submitted Sample ID column in plate_loading ",
                 "to join grouping covariates on. Available columns: ",
                 paste(colnames(processed$plate_loading), collapse = ", ")),
          type = "error", duration = 15)
      } else {
        join_by_vec <- setNames(sub_col, target_key)
        before <- nrow(processed$plate_loading)
        processed$plate_loading <- processed$plate_loading %>%
          left_join(parsed_lookup, by = join_by_vec)
        
        # If the row count grew, covariates fanned out (non-unique per id).
        if (nrow(processed$plate_loading) != before)
          showNotification(
            paste0("Row count changed after join (", before, " -> ",
                   nrow(processed$plate_loading),
                   "). Grouping covariates are fanning out; results may be unreliable."),
            type = "warning", duration = 10)
        
        # Confirm the covariate columns actually populated. All-NA behaves like
        # no grouping, can potentially fail silently.
        na_all <- vapply(rv$parsed_cols, function(cc)
          all(is.na(processed$plate_loading[[cc]])), logical(1))
        if (any(na_all))
          showNotification(
            paste("These covariates are all NA after the join and will NOT group:",
                  paste(rv$parsed_cols[na_all], collapse = ", "),
                  " check that the Submitted Sample IDs match between the Acquired Sample ID",
                  "file and plate_loading output."),
            type = "error", duration = 15)
      }
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
    
    shinyjs::show(session$ns("plate_section"))
    shinyjs::hide(session$ns("download_section"))
    showNotification("Plate meta data generated.", type = "message")
  })
  
  output$lc_inputs_ui <- renderUI({
    ms <- rv$ms_methods_detected
    if (length(ms) == 0) return(tags$em("No MS methods detected."))
    lapply(seq_along(ms), function(i) {
      fluidRow(
        column(2, tags$p(tags$strong(ms[i]),
                         style = "margin-top: 32px; color: #0f5e6d;")),
        column(5,
               textInput(session$ns(paste0("lc_method_", i)),
                         label = paste0("LC Method for ", ms[i], ":"),
                         placeholder = "type here")),
        column(5,
               textInput(session$ns(paste0("sciex_name_", i)),
                         label = paste0("MS Method for ", ms[i], ":"),
                         placeholder = "type here"))
      )
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
    
    # Full SciexOS method names, keyed to the same shorthand MS methods.
    # Empty entries fall back to the shorthand inside generate_sequence().
    sciex_vals <- sapply(seq_along(ms), function(i) {
      val <- input[[paste0("sciex_name_", i)]]
      if (is.null(val)) "" else trimws(val)
    })
    sciex_map <- setNames(sciex_vals, ms)
    
    
    # Only pass saved level orders for variables that are still checked.
    checked      <- input$group_by_cols
    level_orders <- rv$level_orders
    if (length(level_orders) > 0 && !is.null(checked))
      level_orders <- level_orders[names(level_orders) %in% checked]
    
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
        sciex_names   = sciex_map,
        qc_blank_interval       = input$qc_blank_interval,
        group_by_cols           = input$group_by_cols,
        level_orders            = level_orders,
        randomize               = !isTRUE(input$no_randomize),
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
    shinyjs::show(session$ns("download_section"))
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
  })
}