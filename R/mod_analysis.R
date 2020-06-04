#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2,
        numericInput(
          inputId = ns("pen"),
          label = "Penalty",
          value = 15
        ),

        # Type input
        selectInput(
          inputId = ns("graphType"),
          label = "Graph Type",
          choices = c("std", "isotonic", "updown", "relevant")
        ),

        # Generate graph
        actionButton(inputId = ns("updateGraph"), label = "Update graph with above parameters"),
        actionButton(inputId = ns("updateGraph-fromData"), label = "Update graph from uploaded file"),
        HTML("<hr>"),

        # What cost model?
        selectInput(
          inputId = ns("gfpopType"),
          label = "Cost model",
          choices = c("mean", "variance", "poisson", "exp", "negbin")
        ),

        # Submit!
        actionButton(inputId = ns("runGfpop"), label = "Run gfpop!"),
        tags$br(), tags$br(),
        "Note: for testing, using type = mean"
      ),
      column(
        5,
        visNetworkOutput(ns("gfpopGraph"))
      ),
      column(
        5,
        plotlyOutput(ns("gfpopPlot")) %>% withSpinner(type = 6)
      )
    )
  )
}

#' analysis Server Function
#'
#' @noRd
#' @import shiny
#' @importFrom plotly ggplotly 
#' @import gfpop
mod_analysis_server <- function(input, output, session, gfpop_data) {
  ns <- session$ns

  # Initialize main graphdf object
  graphdf <- reactiveValues(graph = gfpop::graph(
    penalty = as.double(15),
    type = "std"
  ))
  
  # If gfpop_data provides a graph, we should use that
  observeEvent(eventExpr = input$`updateGraph-fromData`, {
    graphdf$graph <- gfpop::graph(gfpop_data$graph_input)
  })
    
  # When the "Update graph with above parameters" button is pressed, update graph
  graphdf_default <- observeEvent(eventExpr = input$updateGraph, {
    graphdf$graph <- gfpop::graph(
      penalty = as.double(isolate(input$pen)),
      type = isolate(input$graphType)
    )
  })

  # From the current input data and graph, generate changepoint results
  changepointdf <- reactive({
    req(gfpop_data$primary_input)
    graphdf$graph <- select_graph_columns(graphdf$graph)
    generate_changepoint(gfpop_data$primary_input$Y, graphdf$graph, input$gfpopType)
  })
  
  # Generate a visualization of the current constraint graph
  output$gfpopGraph <- renderVisNetwork({
    # Only run with the "Run gfpop!" button is pressed
    input$runGfpop
    
    # Convert the df into a list, pass that to visNetwork
    graph_data <- graphdf_to_visNetwork(graphdf$graph)
    generate_visNetwork(graph_data)
  })

  # Generate the visualization of the data with overlain changepoints
  # TODO: Can we always show the data, and then just overlay changepoints
  # whenever "Run gfpop!" is pressed?
  output$gfpopPlot <- renderPlotly({
    input$runGfpop

    if (!isTruthy(isolate(gfpop_data$primary_input))) {
      validate(FALSE, "Either you haven't uploaded any data, or you haven't pressed the 'run gfpop!' button yet.")
      return(1)
    }
    data_input <- isolate(gfpop_data$primary_input)
    changepoint_data <- isolate(changepointdf())

    changepoint_data_annot <-
      annotate_data_with_changepoint(data_input, changepoint_data)
    
    ggplotly(plot_changepoint(changepoint_data_annot), 
             tooltip = c("X", "Y", "text"))
  })
}
