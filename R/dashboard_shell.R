#' dashboard_shell
#' Build the shared dashboard frame around the tab module UIs.
#' @return A 'shinydashboard::dashboardPage()' used for a Shiny app
#' 
#' @importFrom shiny HTML tags
#' @export
dashboard_shell <- function() {
  tags$style(HTML("
    .main-header .logo, .main-header .navbar { background-color: #157b8d; }
    .main-header .logo:hover { background-color: #126d7d; }
    .main-sidebar { background-color: #0f5e6d; }
    .sidebar-menu > li > a { color: #d6f0f4; }
    .sidebar-menu > li.active > a, .sidebar-menu > li > a:hover {
      background-color: #157b8d; color: #ffffff; border-left-color: #a8dde5;
    }
    .sidebar-menu > li > a .fa { color: #a8dde5; }
    .method-badge { display: inline-flex; align-items: center; background-color: #157b8d; color: white; border-radius: 12px; padding: 4px 10px; margin: 4px; font-size: 13px; font-weight: bold; }
    .method-badge .remove-btn { background: none; border: none; color: white; margin-left: 6px; cursor: pointer; font-size: 14px; padding: 0; line-height: 1; }
    .method-badge .remove-btn:hover { color: #ffcccc; }
    .status-msg { margin-top: 10px; font-weight: bold; font-size: 14px; }
    .status-msg.success { color: #00a65a; }
    .status-msg.error { color: #dd4b39; }
    .selection-active { color: #157b8d; font-weight: bold; font-size: 15px; margin-top: 8px; margin-bottom: 8px; }
    .selection-idle { color: #6c757d; font-weight: bold; font-size: 15px; margin-top: 8px; margin-bottom: 8px; }
    .rank-list-container { background: #f4fafb; border: 1px solid #cfe6ea; border-radius: 6px; padding: 8px; min-height: 40px; }
    .rank-list-item { background: #157b8d; color: #fff; border-radius: 12px; padding: 6px 12px; margin: 4px; font-size: 13px; font-weight: bold; cursor: grab; display: inline-block; }
    .rank-list-item:active { cursor: grabbing; }
    .rank-var-label { font-weight: bold; color: #0f5e6d; margin-top: 10px; }
    .order-saved-msg { color: #00a65a; font-weight: bold; font-size: 13px; margin-top: 6px; }
  "))
}

#' dashboard_ui
#' UI elements for the dashboard
#' 
#' @return Shiny UI element
#' @importFrom shiny icon
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem sidebarMenu tabItems
#' @importFrom shinyjs useShinyjs

dashboard_ui <- function() {
  dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "50px",
               style = "margin-right: 10px; vertical-align: left;"),
      "Comp Tools for Mass Spec"
    ),
    titleWidth = 400
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TAS Accelerator", tabName = "acquired_tab", icon = icon("cogs")),
      menuItem("Sequence Generator", tabName = "plate_tab", icon = icon("th"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    dashboard_shell(),
    tabItems(
      tas_accelerator_ui("tas_accelerator"),
      sequence_generator_ui("sequence_generator")
    )
  )
)}
