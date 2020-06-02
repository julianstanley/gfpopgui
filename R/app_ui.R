#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import plotly
#' @import visNetwork
#' @import shinycssloaders
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      navbarPage(id = "tabs",
        title = "gfpop",
        collapsible = TRUE,
        # include the UI for each tab
        source(system.file("R/ui/tab_home.R", package = "gfpopgui"))$value,
        source(system.file("R/ui/tab_analysis.R", package = "gfpopgui"))$value,
        tabPanel("Sharing"),
        tabPanel("Help")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gfpopgui"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
