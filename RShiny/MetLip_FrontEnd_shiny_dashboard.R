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

library(MetLipAutomation)



# define the UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "MetLip Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TAS Submitted Samples", tabName = "sample_meta", icon = icon("search")),
      menuItem("Sample Acquisition IDs", tabName = "acquisition_ids", icon = icon("table")),
      menuItem("Plate Meta Data", tabName = "plate_meta", icon = icon("vial")),
      menuItem("SciexOS Sequence", tabName = "sequence_data", icon = icon("dna")))),
  
  dashboardBody(
    # define UI for uploading the TAS submitted samples file
    tabItems(
      tabItem(tabName = "sample_meta",
              fluidRow(
                box(title = "Upload TAS Submitted Samples", status = "primary", solidHeader = TRUE, 
                    fileInput("sample_file", "Choose Excel File", accept = c(".xlsx", ".xls")),
                    DTOutput("sample_table")))),
      
      # define UI for acquisition IDs tab
      tabItem(tabName = "acquisition_ids",
              fluidRow(
                box(title = "Acquisition IDs Per Month", status = "primary", solidHeader = TRUE, 
                    plotlyOutput("acq_ids_plot")),
                
                box(title = "Generate Sample Acquisition IDs", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxGroupInput("ms_method_selection", "Select MS Methods:", 
                                       choices = c("TCM-F5","TCM-IP", "LM", "TBL", "SCFA", "Bile-Acids", "Custom"), 
                                       selected = c("TCM-F5","TCM-IP", "LM", "TBL", "SCFA", "Bile-Acids", "Custom")),
                    actionButton("generate_acq_ids", "Generate Data"),
                    downloadButton("download_acq_ids", "Download CSV"),
                    downloadButton("download_acq_ids_excel", "Download Excel"),
                    div(style = "overflow-x: auto;"),
                    DTOutput("acq_ids_table")))),
      
      # define UI for generating plate meta data
      tabItem(tabName = "plate_meta",
              fluidRow(
                box(title = "Generate Plate Meta Data", status = "primary", solidHeader = TRUE, 
                    actionButton("generate", "Generate Data"),
                    downloadButton("download_plate", "Download CSV"),
                    downloadButton("download_plate_excel", "Download Excel"),
                    DTOutput("plate_table")))),
      
      # define UI for generating SciexOS sequence data
      tabItem(tabName = "sequence_data",
              fluidRow(
                box(title = "Generate Sequence", status = "primary", solidHeader = TRUE, 
                    actionButton("generate", "Generate Data"),
                    downloadButton("download_sequence", "Download CSV"),
                    DTOutput("sequence_table")))
      )
    )
  )
)

# write the server logic. The server function controls what happens when the users interact with the dashboard.
server <- function(input, output, session, sample_data) {
  sample_data <- reactiveVal()
  acq_ids_data <- reactiveVal()
  plate_data <- reactiveVal()
  historical_data <- reactiveVal(data.frame(Date = as.Date(character()), Count = integer()))
  
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
    
    # Update historical data
    new_entry <- data.frame(month = floor_date(Sys.Date(), "month"), Count = nrow(acq_data))
    updated_data <- bind_rows(historical_data(), new_entry) %>% 
      group_by(month) %>%
      summarise(Count = sum(Count), .groups = 'drop')
    historical_data(updated_data)
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
  
  output$acq_ids_plot <- renderPlotly({
    req(historical_data())
    plot_ly(historical_data(), x = ~month, y = ~Count, type = 'bar', name = 'Acquisition IDs') %>%
      layout(title = "Total Acquisition IDs Per Month",
             xaxis = list(title = "Month-Year", type = "date", tickformat = "%b %Y", dtick = "M1"),
             yaxis = list(title = "Total IDs"),
             plot_bgcolor = "#f5f5f5", 
             paper_bgcolor = "#ffffff", 
             font = list(family = "Arial", size = 12, color = "#333333"))
  })
  
  
  
  
  # logic corresponding to the generation of plate meta data
  observeEvent(input$generate, {
    req(acq_ids_data())
    plate_meta_data <- generate_plate_meta_data(acq_ids_data(), randomize = FALSE) # calls on our previously defined function
    plate_data(plate_meta_data)
  })
  
  output$plate_table <- renderDT({
    req(plate_data())
    datatable(plate_data())
  })
  
  output$download_plate <- downloadHandler(
    filename = function() { "plate_meta_data.csv" },
    content = function(file) {
      req(plate_data())
      write.csv(plate_data(), file, row.names = FALSE)
    }
  )
  
  output$download_plate_excel <- downloadHandler(
    filename = function() { "plate_meta_data.xlsx" },
    content = function(file) {
      req(plate_data())
      write_xlsx(plate_data(), file)
    }
  )
}

shinyApp(ui, server)
