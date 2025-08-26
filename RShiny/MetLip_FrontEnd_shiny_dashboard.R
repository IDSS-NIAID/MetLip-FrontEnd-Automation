library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(writexl)
library(purrr)
library(plotly)
library(lubridate)
library(stringr)
library(zip)

if(FALSE)
  devtools::install_github('IDSS-NIAID/MetLip-FrontEnd-Automation')
library(MetLipAutomation)



# define the UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "MetLip Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "sample_meta", icon = icon("search")),
      menuItem("File Pre-Process", tabName = "validation", icon = icon("cog")),
      menuItem("Plate Loading Data", tabName = "plate_meta", icon = icon("gears")),
      menuItem("SciexOS Sequence", tabName = "sequence_data", icon = icon("coffee")))),
  
  dashboardBody(
    # define UI for uploading the TAS file with submitted sample names and acquisition IDs
    tabItems(
      tabItem(tabName = "sample_meta",
              fluidRow(
                box(title = "Upload TAS File", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("sample_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                    DTOutput("sample_table")))),
      
      
      # define UI for file validation
      tabItem(
        tabName = "validation",
        fluidRow(
          box(
            title = "File Validation", status = "primary", solidHeader = TRUE, width = 12,
            textInput("project_name", "Project name / ID:", placeholder = "e.g., ABC123"),
            actionButton("validate_file", "Validation Step"),
            div(style = "overflow-x: auto;"),
            DTOutput("validation_table")
          )
        )
      ),
      
      
      # define UI for generating plate meta data
      tabItem(tabName = "plate_meta",
              fluidRow(
                box(title = "Generate Plate Meta Data", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("generate", "Generate Data"),
                    downloadButton("download_plate", "Download CSV"),
                    downloadButton("download_plate_excel", "Download Excel"),
                    DTOutput("plate_table")))),
      
      
      # define UI for generating SciexOS sequence data
      tabItem(tabName = "sequence_data",
              fluidRow(
                box(title = "Generate Sequence", status = "primary", solidHeader = TRUE, 
                    numericInput(
                      inputId = "injection_vol",
                      label = "Injection Volume (uL)",
                      value = 5,
                      min = 2,
                      max = 20, 
                      step = 0.5,
                      width = "25%"),
                    downloadButton("download_sequence_zip", "Download sequence ZIP"),
                    actionButton("generate_sequence", "Generate and Download Sequences"),
                    DTOutput("sequence_table")
          )
        )
      )
    )
  )
)



# write the server logic. The server function controls what happens when the users interact with the dashboard.
server <- function(input, output, session, sample_data) {
  sample_data <- reactiveVal()
  file_preprocessed <- reactiveVal()
  acq_ids_data <- reactiveVal()
  plate_data <- reactiveVal()
  processed_plate_data <- reactiveVal()
  sequence_data <- reactiveVal()
  
  seq_paths <- reactiveVal(NULL)
  seq_dir   <- reactiveVal(NULL)
  
  # logic for uploading the TAS File
  observeEvent(input$sample_file, {
    req(input$sample_file)
    df <- read_excel(input$sample_file$datapath)
    file_preprocessed(df)
  })
  
  # view the pre-processed TAS File
  output$sample_table <- renderDT({
    req(file_preprocessed())
    datatable(file_preprocessed(), options = list(scrollX = TRUE))
  })
  
  
  # logic for valiudation step
  observeEvent(input$validate_file, {
    req(file_preprocessed(), nzchar(trimws(input$project_name)))
    
    processed <- append_project_name(
      df = file_preprocessed(),
      project_name = input$project_name,  # repeats for all rows
      col = "Project_ID",                 # change if you want a different name
      overwrite = TRUE                    # or FALSE if you prefer to keep existing
    )
    
    acq_ids_data(processed)
  })
  
  output$validation_table <- renderDT({
    req(acq_ids_data())
    datatable(acq_ids_data(), options = list(scrollX = TRUE))
  })
  
  
  # logic corresponding to the generation of plate meta data
  observeEvent(input$generate, {
    req(acq_ids_data())
    plate_meta_data <- generate_plate_meta_data(acq_ids_data(), randomize = FALSE) # calls on our previously defined function
    plate_data(plate_meta_data)
    processed_data <- process_plate_data(plate_meta_data, acq_ids_data())
    processed_plate_data(processed_data)
  })
  
  output$plate_table <- renderDT({
    req(plate_data())
    datatable(plate_data())
  })
  
  output$download_plate <- downloadHandler(
    filename = function() { "processed_plate_data.csv" },
    content = function(file) {
      req(processed_plate_data())
      write.csv(processed_plate_data()$plate_loading, file, row.names = FALSE)
    }
  )
  
  output$download_plate_excel <- downloadHandler(
    filename = function() { "processed_plate_data.xlsx" },
    content = function(file) {
      req(processed_plate_data())
      write_xlsx(processed_plate_data()$plate_loading, file)
    }
  )
  
  
  # # logic corresponding to the generation of SciexOS sequence data
  # observeEvent(input$generate_sequence, {
  #   req(processed_plate_data())
  #   generate_sequence( 
  #     processed_plate_data()$plate_loading,
  #     processed_plate_data()$qc_plate_data,
  #     processed_plate_data()$blank_plate_data,
  #     processed_plate_data()$project_id,
  #     injection_vol = input$injection_vol
  #   ) # calls on our previously defined function                        
  # })
  
  
  # --- UPDATED: logic corresponding to the generation of SciexOS sequence data
 observeEvent(input$generate_sequence, {
    req(processed_plate_data())

    td <- tempfile("seq_")
    dir.create(td, recursive = TRUE)

    paths <- generate_sequence(
      processed_plate_data()$plate_loading,
      processed_plate_data()$qc_plate_data,
      processed_plate_data()$blank_plate_data,
      processed_plate_data()$project_id,
      injection_vol = input$injection_vol,
      output_dir    = td
    )

    validate(need(length(paths) > 0, "No sequence files were generated."))
    seq_paths(paths)
    seq_dir(td)
    showNotification(sprintf("Generated %d file(s). Ready to download.", length(paths)), type = "message")
  })

  # Download ZIP of generated sequence files
  output$download_sequence_zip <- downloadHandler(
    filename = function() {
      pid <- processed_plate_data()$project_id
      sprintf("Sequences_%s_%s.zip", format(Sys.Date(), "%Y%m%d"), pid)
    },
    content = function(file) {
      req(seq_paths(), seq_dir())

      # Use relative names with root= for zip::zipr
      rel  <- basename(seq_paths())
      full <- file.path(seq_dir(), rel)
      if (!all(file.exists(full))) {
        stop("One or more sequence files are missing; please regenerate before downloading.")
      }

      # Create zip in a temp file, then stream to browser
      tmpzip <- tempfile(fileext = ".zip")
      zip::zipr(zipfile = tmpzip, files = rel, root = seq_dir())

      ok <- file.copy(tmpzip, file, overwrite = TRUE)
      if (!ok) stop("Failed to stream ZIP to client.")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
