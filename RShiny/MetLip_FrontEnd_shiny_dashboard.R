library(shiny)
library(shinydashboard)
library(jsonlite)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(readxl)
library(zip)
library(sortable)


if (FALSE)
  devtools::install_github("IDSS-NIAID/MetLip-FrontEnd-Automation")
library(MetLipAutomation)


source(file.path("R", "tas_client.R"))
source(file.path("RShiny", "dashboard_shell.R"))
source(file.path("RShiny", "modules", "tas_accelerator_module.R"))
source(file.path("RShiny", "modules", "sequence_generator_module.R"))

tas_api <- new_tas_client()

ui <- dashboard_shell(
  tas_accelerator_ui("tas"),
  sequence_generator_ui("sequence")
)

server <- function(input, output, session) {
  tas_samples <- tas_accelerator_server("tas", tas_api)
  sequence_generator_server("sequence", tas_samples, tas_api$detect_sub_id_col)
}

shinyApp(ui, server)