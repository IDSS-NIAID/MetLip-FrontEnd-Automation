library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(readxl)
library(zip)
library(sortable)
library(MetLipAutomation)

sortable::enable_modules()

ui <- dashboard_ui()

server <- function(input, output, session) {
  app_state <- reactiveValues(
    request_id = NULL,
    df_submitted = NULL,
    df_generated = NULL,
    df_confirmed = NULL,
    submitted_id_col = NULL
  )
  tas_accelerator_server("tas_accelerator", app_state)
  sequence_generator_server("sequence_generator", app_state)
}

shinyApp(ui, server)
