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
      tabItem(tabName = "validation",
              fluidRow(
                box(title = "File Validation", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("validate_file", "Validation Step"),
                    div(style = "overflow-x: auto;"),
                    DTOutput("validation_table")))),
      
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
  
  
  # logic to validate/process TAS File
  observeEvent(input$validate_file, {
    req(file_preprocessed())
    processed_data <- process_acquisition_ids_reduced(file_preprocessed()) # calls on our previously defined (reduced) function
    acq_ids_data(processed_data)
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
  
  
  # logic corresponding to the generation of SciexOS sequence data
  observeEvent(input$generate_sequence, {
    req(processed_plate_data())
    generate_sequence( 
      processed_plate_data()$plate_loading,
      processed_plate_data()$qc_plate_data,
      processed_plate_data()$blank_plate_data,
      processed_plate_data()$project_id,
      injection_vol = input$injection_vol
    ) # calls on our previously defined function                        
  })
}

shinyApp(ui, server)
