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
  source(file.path("R", "server", "tab_home.R"), local = TRUE)$value
  source(file.path("R", "server", "tab_analysis.R"), local = TRUE)$value
}
