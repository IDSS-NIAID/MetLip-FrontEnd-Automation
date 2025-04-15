library(shiny)
library(shinydashboard)
library(jose)
library(httr)
library(readxl)
library(DT)

# Shared secret - must match what's used in plumber.R
my_secret <- charToRaw(Sys.getenv("SECRET_KEY"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Secure File Viewer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search Tab", tabName = "search_tab", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "search_tab",
              fluidRow(
                box(
                  title = "Secure ID Search & File Viewer",
                  width = 6,
                  textInput("id_input", "Enter ID:", ""),
                  actionButton("search_btn", "Search"),
                  br(),
                  h4("📜 Search History"),
                  verbatimTextOutput("search_history")
                ),
                box(
                  title = "🔍 Excel File Preview",
                  width = 12,
                  DTOutput("excel_table")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Store searched IDs during session
  search_history <- reactiveValues(ids = character())
  
  observeEvent(input$search_btn, {
    req(input$id_input)
    
    # Append current ID to history
    search_history$ids <- c(search_history$ids, input$id_input)
    
    # Create a JWT claim object
    claim <- jwt_claim(
      claim = list(
        id = input$id_input,
        timestamp = Sys.time()
      ),
      sub = "shiny-user",       # Optional: JWT subject
      aud = "shiny-dashboard",  # Optional: intended audience
      iss = "your-app"          # Optional: issuer
    )
    
    # Encode it using the shared secret
    jwt <- jwt_encode_hmac(claim, my_secret)
    
    # Send POST request with JWT to your local API
    res <- tryCatch({
      POST(
        url = "http://localhost:8000/secure-id-check",
        add_headers(Authorization = paste("Bearer", jwt)),
        encode = "json"
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
    
    # Process API response
    if (inherits(res, "response") && res$status_code == 200) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(content(res, "raw"), temp_file)
      
      df <- tryCatch({
        read_excel(temp_file)
      }, error = function(e) {
        showNotification("Could not read Excel file", type = "error")
        NULL
      })
      
      output$excel_table <- renderDT({
        if (!is.null(df)) datatable(df)
      })
    } else {
      showNotification("Authentication failed or file not available", type = "error")
    }
    
    # Update and show search history
    output$search_history <- renderPrint({
      print(search_history$ids)
    })
  })
}

# Launch the app
shinyApp(ui, server)
