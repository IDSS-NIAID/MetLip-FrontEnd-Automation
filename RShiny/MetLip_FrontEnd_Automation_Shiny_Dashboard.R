library(shiny)
library(dplyr)
library(DT)
library(writexl)

# Define UI for application
ui <- fluidPage(
  titlePanel("Meta Data Generator Dashboard (Beta)"),
  sidebarLayout(
    sidebarPanel(
      textInput("tas_id", "Enter TAS ID:"),
      textInput("metlip_id", "Enter Sub (MetLip) ID:"),
      textInput("conditions", "Enter conditions (Comma Separated):"),
      checkboxGroupInput("matrices", "Select Matrices:", 
                         choices = c("Plasma", "Kidney", "Liver", "Lung", "Brain", "Duodenum", "Ileum", "Jejunum", "Feces", "Cells", "Supernatant")),
      numericInput("replicates", "Enter number of replicates:", value = 1, min = 1),
      numericInput("num_subjects", "Enter number of subjects:", value = 1, min = 1),
      actionButton("generate", "Generate Meta Data"),
      actionButton("clear", "Clear Fields"),
      downloadButton("downloadData", "Download Excel")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  meta_data <- reactiveVal()
  
  observeEvent(input$generate, {
    conditions <- unlist(strsplit(input$conditions, ","))
    matrices <- input$matrices
    
    full_data <- list()
    
    for (matrix in matrices) {
      matrix_data <- expand.grid(
        TAS_id = input$tas_id,
        MetLip_id = input$metlip_id,
        Subject = 1:input$num_subjects,
        Condition = conditions,
        Matrix = matrix,
        Replicate = 1:input$replicates
      )
      
      full_data[[matrix]] <- matrix_data
    }
    
    temp_1 <- bind_rows(full_data) %>%
      group_by(Matrix) %>%
      arrange(Subject, .by_group = TRUE) %>%
      mutate(Vial = paste(sprintf("%03d", row_number()), sep = "_")) %>%
      ungroup()
    
    meta_data(temp_1)
  })
  
  observeEvent(input$clear, {
    updateTextInput(session, "metlip_id", value = "")
    updateTextInput(session, "conditions", value = "")
    updateCheckboxGroupInput(session, "matrices", selected = character(0))
    updateNumericInput(session, "replicates", value = 1)
    updateNumericInput(session, "num_subjects", value = 1)
    meta_data(NULL)
  })
  
  output$table <- renderDT({
    req(meta_data())
    datatable(meta_data())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", input$metlip_id, "_Sample_Meta_Data.xlsx")
    },
    content = function(file) {
      write_xlsx(meta_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
