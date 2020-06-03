#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import plotly
#' @import gfpop
#' @import data.table
#' @noRd
app_server <- function(input, output, session) {
  set.seed(123)
  # include logic for each tab
  gfpop_data <- callModule(mod_home_server, "home_ui_1")
  callModule(mod_analysis_server, "analysis_ui_1", gfpop_data)
}
