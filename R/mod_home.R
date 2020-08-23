#' home UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList fluidRow column h1 br h3 includeHTML div
#' fileInput p h4
#' @importFrom DT dataTableOutput
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        includeHTML(system.file("app/www/intro.html", package = "gfpopgui")),
      ),
      column(
        4,
        h3("Upload", align = "center"),
        div(
          class = "well",
          h4("Upload Data"), br(),
          fileInput(
            ns("primary_input"),
            "Choose a file with primary input data (.csv)"
          ),
          p("Or generate random data:"),
          details(
            summary = "Data Generation Options", summary_multiplier = 1,
            content = tagList(
              fluidRow(
                hr(),
                column(
                  12,
                  h4("How many datapoints, and with what standard deviation?")
                ),
                column(
                  6,
                  numericInput(ns("ndata"),
                    label = "# Datapoints",
                    value = 1000
                  )
                ),
                column(6, numericInput(ns("sigma"),
                  label = "sigma", value = 1, min = 0.1
                )),
                column(
                  12,
                  includeHTML(system.file("app/www/generation_snippet.html",
                    package = "gfpopgui"
                  ))
                ),
                column(
                  6,
                  numericInput(ns("nChangepoints"),
                    label = "# Changepoints", value = 5
                  )
                ),
                column(6, numericInput(ns("eChangepoints"),
                  label = "e (log spacing of changepoints",
                  value = 1
                )),
                column(
                  12,
                  h4("What are the changepoint means, the cost model, and gamma?")
                ),
                column(12, textInput(ns("meansChangepoints"),
                  label = "Means (no spaces, comma-seperated, 
                length must equal the # Changepoints"
                )),
                column(6, selectInput(ns("typeChangepoints"),
                  label = "Type", choices = c(
                    "mean", "variance", "poisson", "exp", "negbin"
                  ), selected = "mean"
                )),
                column(6, numericInput(ns("gammaChangepoints"),
                  label = "gamma",
                  value = 1, min = 0, max = 1
                ))
              )
            )
          ),
          br(),
          actionButton(inputId = ns("genData"), label = "Generate!"),
          HTML("<hr>"),
          HTML("<h4>Optional uploads:</h4>"),
          fileInput(
            ns("constraint_graph"),
            "Choose a file with a constraint graph (.csv)"
          ),
          fileInput(
            ns("completed_analysis"),
            "Choose a file with a completed analysis (.Rdata)"
          ),
        ),
        h3("Preview Uploaded Data", align = "center"),
        div(
          class = "well",
          h4("Input data:"),
          dataTableOutput(ns("main_datatable")),
          h4("Constraint graph:"),
          dataTableOutput(ns("graph"))
        )
      )
    )
  )
}

#' Core functionality of the home server function
#' @param input From shiny
#' @param output From shiny
#' @param session From shiny
#' @importFrom shiny reactive isTruthy req observeEvent
#' @importFrom data.table fread
#' @importFrom DT renderDataTable
#' @importFrom shinyalert shinyalert
mod_home_server_core <- function(input, output, session) {
  # Reactive Values ------------------------------------------------------------
  gfpop_data <- reactiveValues(
    graphdata = gfpop::graph(
      penalty = as.double(15),
      type = "std"
    ),
    graphdata_visNetwork = graphdf_to_visNetwork(gfpop::graph(
      penalty = as.double(15),
      type = "std"
    ), edge_ids = c("std_std_null", "std_std_std"))
  )

  # Main Data Input and Preview-------------------------------------------------
  observeEvent(input$completed_analysis, {
    rdata_name <- load(input$completed_analysis$datapath)
    gfpop_data_list <- mget(rdata_name, environment())
    lapply(
      names(gfpop_data_list[[1]]),
      function(x) gfpop_data[[x]] <- gfpop_data_list[[1]][[x]]
    )
  })

  observeEvent(input$primary_input, {
    primary_input <- fread(input$primary_input$datapath)
    if (ncol(primary_input) == 2) {
      tryCatch(
        expr = {
          colnames(primary_input) <- c("X", "Y")
          gfpop_data$main_data <- primary_input
        },
        error = function(e) {
          shinyalert(
            title = "Unknown error",
            text = paste0("Some unknown error occured while,
                                     trying to process your input data: ", e)
          )
        }
      )
    } else {
      shinyalert(
        title = "Incorrect input shape",
        text = "Input does not have two columns. 
                                     Please upload data with two columns."
      )
    }
  })

  # Render a datatable of the main data
  output$main_datatable <- DT::renderDataTable(gfpop_data$main_data,
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  # Generate some random data when the button is pressed
  genData_func <- observeEvent(input$genData, {
    parameters <- rep(c(1,2), length.out = input$nChangepoints)
    # Make changepoint means
    if (input$meansChangepoints == "") {
    } else {
      tryCatch(
        expr = {
          parameters <- as.numeric(strsplit(input$meansChangepoints, ",")[[1]])
        },
        error = function(e) {
          shinyalert(
            title = "Changepoint means malformed",
            text = paste0("Could not form changepoint means. Is your changepoint
                        mean input malformed?, error: ", e),
            type = "error"
          )
        }
      )
    }

    # Ensure that we have the same number of changepoints and changepoint means
    if (length(parameters) != input$nChangepoints) {
      shinyalert(
        title = "Parameters do not match.",
        text = paste0("You asked for ", input$nChangepoints, " changepoints,
                          but only provided ", length(parameters), " parameters."),
        type = "error"
      )
      return(1)
    }

    # Make changepoint locations
    changepoint_locs <- make_changepoint_locs(input$nChangepoints, input$eChangepoints)

    # Try to generate data with dataGenerator
    tryCatch(
      expr = {
        primary_input <- data.table(
          X = 1:input$ndata,
          Y = dataGenerator(input$ndata,
            changepoints = changepoint_locs,
            parameters = parameters, type = input$typeChangepoints,
            sigma = input$sigma, gamma = input$gammaChangepoints
          )
        )
        gfpop_data$main_data <- primary_input
      },
      error = function(e) {
        shinyalert(
          title = "Data Generation Error",
          text = paste0("Some unknown error occured during data generation. error: ", e),
          type = "error"
        )
      }
    )
  })


  # Graph Data Input and Preview -----------------------------------------------
  set_graph_data <- reactive({
    if (isTruthy(input$constraint_graph)) {
      tryCatch(
        expr = {
          graph_input <- fread(input$constraint_graph$datapath, sep = ",", stringsAsFactors = F)
          gfpop_data$graph_input <- graph_input
          gfpop_data$graphdata <- gfpop::graph(gfpop_data$graph_input)
          gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata,
            edge_ids = paste0(
              gfpop_data$graphdata$state1, "_",
              gfpop_data$graphdata$state2, "_",
              gfpop_data$graphdata$state
            )
          )
        },
        error = function(e) {
          shinyalert(
            title = "Unknown error",
            text = paste0("Some unknown error occured
                                                   while trying to upload your
                                                   data: ", e)
          )
        }
      )
    }
  })

  output$graph <- DT::renderDataTable(
    {
      set_graph_data()
      gfpop_data$graphdata
    },
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  return(gfpop_data)
}

#' home Server Function
#' @param id The id of this module
#' @importFrom shiny moduleServer
#' @export
mod_home_server <- function(id) {
  moduleServer(
    id,
    mod_home_server_core
  )
}
