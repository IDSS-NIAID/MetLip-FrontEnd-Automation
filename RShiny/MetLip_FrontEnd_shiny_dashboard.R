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
      menuItem("Sample Acquisition IDs", tabName = "acquisition_ids", icon = icon("cog")),
      menuItem("Plate Loading Data", tabName = "plate_meta", icon = icon("gears")),
      menuItem("SciexOS Sequence", tabName = "sequence_data", icon = icon("coffee")))),
  
  dashboardBody(
    # define UI for uploading the TAS submitted samples file
    tabItems(
      tabItem(tabName = "sample_meta",
              fluidRow(
                box(title = "Upload TAS Submitted Samples", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("sample_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                    DTOutput("sample_table")))),
      
      # define UI for acquisition IDs tab
      tabItem(tabName = "acquisition_ids",
              fluidRow(
                box(title = "Generate Sample Acquisition IDs", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxGroupInput("ms_method_selection", "Select MS Methods:", 
                                       choices = c("TCM-F5","TCM-IP", "LM", "TBL", "SCFA", "Bile-Acids", "Custom"), 
                                       selected = c()),
                    actionButton("generate_acq_ids", "Generate Data"),
                    downloadButton("download_acq_ids", "Download CSV"),
                    downloadButton("download_acq_ids_excel", "Download Excel"),
                    div(style = "overflow-x: auto;"),
                    DTOutput("acq_ids_table")))),
      
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
  acq_ids_data <- reactiveVal()
  plate_data <- reactiveVal()
  processed_plate_data <- reactiveVal()
  sequence_data <- reactiveVal()
  
  
  # logic for uploading data
  observeEvent(input$sample_file, {
    req(input$sample_file)
    df <- read_excel(input$sample_file$datapath)
    sample_data(df)
  })
  
  output$sample_table <- renderDT({
    req(sample_data())
    datatable(sample_data())
  })
  
  
  # logic for processing acquisition ids
  observeEvent(input$generate_acq_ids, {
    req(sample_data())
    selected_ms_methods <- input$ms_method_selection
    acq_data <- process_acquisition_ids(sample_data(), selected_ms_methods) # calls on our previously defined function
    acq_ids_data(acq_data)
  })
  
  output$acq_ids_table <- renderDT({
    req(acq_ids_data())
    datatable(acq_ids_data(), options = list(scrollX = TRUE))
  })
  
  output$download_acq_ids <- downloadHandler(
    filename = function() { "acquisition_ids.csv" },
    content = function(file) {
      req(acq_ids_data())
      write.csv(acq_ids_data(), file, row.names = FALSE)
    }
  )
  
  output$download_acq_ids_excel <- downloadHandler(
    filename = function() { "acquisition_ids.xlsx" },
    content = function(file) {
      req(acq_ids_data())
      write_xlsx(acq_ids_data(), file)
    }
  )
  
  
  # logic corresponding to the generation of plate meta data
  observeEvent(input$generate, {
    req(acq_ids_data())
    plate_meta_data <- generate_plate_meta_data(acq_ids_data(), randomize = FALSE) # calls on our previously defined function
    plate_data(plate_meta_data)
    processed_data <- process_plate_data(plate_meta_data, acq_ids_data())
    processed_plate_data(processed_data)
  })
  
  output$plate_table <- renderDT({
    req(processed_plate_data())
    datatable(processed_plate_data()$plate_loading)
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
      injection_vol = 5
    ) # calls on our previously defined function                        
  })
}

shinyApp(ui, server)
