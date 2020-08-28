#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList fluidPage navbarPage tabPanel
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      theme = shinytheme("lumen"),
      navbarPage(
        id = "tabs",
        title = "gfpop",
        collapsible = TRUE,
        # include the UI for each tab
        tabPanel(
          "Home",
          mod_home_ui("home_ui_1")
        ),
        tabPanel(
          "Analysis",
          mod_analysis_ui("analysis_ui_1")
        ),
        tabPanel(
          "Sharing",
          mod_sharing_ui("sharing_ui_1")
        ),
        tabPanel("Help",
                 includeHTML(system.file("app/www/help.html", package = "gfpopgui")))
      ),
      includeHTML(system.file("app/www/footer.html", package = "gfpopgui"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  add_resource_path(
    "img", system.file("app/img", package = "gfpopgui")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gfpopgui"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    useShinyjs(),
    shinyalert::useShinyalert()
  )
}
