#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import plotly
#' @import visNetwork
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      navbarPage(
        "gfpop",
        tabPanel(
          "Home",
          # TODO: Put back in module file
          tagList(
            fluidRow(
              column(
                8,
                h1("Welcome to gfpop", align = "center"),
                HTML("<h4 style='text-align: center'> An analysis tool for graph-constrained <br> changepoint detection </h4>"),
                br(), br(),
                h3("Getting started: overview and instructions"),
                includeHTML("../inst/lorem.html")
              ),
              column(
                4,
                h3("Upload user data", align = "center"),
                div(
                  class = "well",
                  HTML("<h4>Basic uploads:</h4><br>"),
                  fileInput("primary_input", "Choose a file with primary input data (.csv)"),
                  p("Or generate data (temporary):"),
                  fluidRow(
                    column(6, numericInput(inputId = "ndata", label = "# Datapoints", value = 1000)),
                    column(6, numericInput(inputId = "sigma", label = "sigma", value = 1, min = 0.1)),
                  ),
                  actionButton(inputId = "genData", label = "Generate!"),
                  HTML("<hr>"),
                  HTML("<h4>Optional/advanced uploads:</h4><br>"),
                  fileInput("[not working yet] constraint_graph", "Choose a file with a constraint graph (.csv)"),
                  fileInput("[not working yet] completed_analysis", "Choose a file with a completed analysis (.Rdata)"),
                ),
                h3("Uploaded data:", align = "center"),
                div(
                  class = "well",
                  h4("Input data:"),
                  dataTableOutput("main_data"),
                  h4("Constraint graph:"),
                  dataTableOutput("graph")
                )
              )
            )
          )
        ),
        tabPanel(
          "Analysis",
          fluidRow(
            column(
              2,
              numericInput(
                inputId = "pen",
                label = "Penalty",
                value = 15
              ),

              # Type input
              selectInput(
                inputId = "graphType",
                label = "Graph Type",
                choices = c("std", "isotonic", "updown", "relevant")
              ),

              # Submit!
              actionButton(inputId = "runGfpop", label = "Run gfpop!"),
              p("Note: for testing, type =")
            ),
            column(
              5,
              visNetworkOutput("gfpopGraph")
            ),
            column(
              5,
              plotlyOutput("gfpopPlot")
            )
          )
        ),
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
