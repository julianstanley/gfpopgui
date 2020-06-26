#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny callModule
#' @noRd
#' @export
app_server <- function(input, output, session) {
  set.seed(123)
  # include logic for each tab
  gfpop_data <- mod_home_server("home_ui_1")
  mod_analysis_server("analysis_ui_1", gfpop_data)
}
