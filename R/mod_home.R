#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column h1 br h3 includeHTML div 
#' fileInput p h4
#' @importFrom DT dataTableOutput
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        h1("Welcome to gfpop", align = "center"),
        HTML("<h4 style='text-align: center'> An analysis tool for graph-constrained <br> changepoint detection </h4>"),
        br(), br(),
        h3("Getting started: overview and instructions"),
        includeHTML(system.file("app/www/lorem.html", package = "gfpopgui")),
      ),
      column(
        4,
        h3("Upload user data", align = "center"),
        div(
          class = "well",
          HTML("<h4>Basic uploads:</h4><br>"),
          fileInput(ns("primary_input"), "Choose a file with primary input data (.csv)"),
          p("Or generate data (temporary):"),
          fluidRow(
            column(6, numericInput(inputId = ns("ndata"), label = "# Datapoints", value = 1000)),
            column(6, numericInput(inputId = ns("sigma"), label = "sigma", value = 1, min = 0.1)),
          ),
          actionButton(inputId = ns("genData"), label = "Generate!"),
          HTML("<hr>"),
          HTML("<h4>Optional/advanced uploads:</h4><br>"),
          fileInput(ns("constraint_graph"), "Choose a file with a constraint graph (.csv)"),
          fileInput(ns("completed_analysis"), "[Pending] Choose a file with a completed analysis (.Rdata)"),
        ),
        h3("Uploaded data:", align = "center"),
        div(
          class = "well",
          h4("Input data:"),
          DT::dataTableOutput(ns("main_datatable")),
          h4("Constraint graph:"),
          DT::dataTableOutput(ns("graph"))
        )
      )
    )
  )
}

#' home Server Function
#'
#' @noRd
#' @importFrom shiny reactive isTruthy req observeEvent
#' @importFrom data.table fread
#' @importFrom DT renderDataTable
#' @export
mod_home_server <- function(input, output, session) {
  ns <- session$ns
  
  # Set variables --------------------------------------------------------------
  gfpop_data <- reactiveValues(
    graphdata = gfpop::graph(
    penalty = as.double(15),
    type = "std"
  ),
  graphdata_visNetwork = graphdf_to_visNetwork(gfpop::graph(
    penalty = as.double(15),
    type = "std"
  ))
  )

  # Main Data Input and Preview-------------------------------------------------
  set_main_data <- reactive({
    if (isTruthy(input$primary_input)) {
      primary_input <- fread(input$primary_input$datapath, header = F, stringsAsFactors = FALSE)
      colnames(primary_input) <- c("X", "Y")
      gfpop_data$main_data <- primary_input
    }
  })
  
  # TODO: main_data should also accept .Rdata from `input$completed_analysis`
  # Parse the primary input data and return back a DataTable
  output$main_datatable <- DT::renderDataTable(
    {
      set_main_data()
      gfpop_data$main_data
    },
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )
  
  # Generate some data if users ask for it (temporary, for testing)
  observeEvent(input$genData, {
    primary_input <- data.frame(
      X = 1:input$ndata,
      Y = dataGenerator(input$ndata, c(0.1, 0.3, 0.5, 0.8, 1), 
                        c(1, 2, 1, 3, 1), sigma = input$sigma)
    )
    gfpop_data$main_data <- primary_input
  })
  
  
  # Graph Data Input and Preview -----------------------------------------------
  set_graph_data <- reactive({
    if (isTruthy(input$constraint_graph)) {
      graph_input <- fread(input$constraint_graph$datapath, sep = ",", stringsAsFactors = F)
      gfpop_data$graph_input <- graph_input
      gfpop_data$graphdata <- gfpop::graph(gfpop_data$graph_input)
      gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata)
    } 
  })
  
  # TODO: main_data should also accept .Rdata from `input$completed_analysis`
  output$graph <- DT::renderDataTable(
    {
      set_graph_data()
      gfpop_data$graphdata
    },
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  return(gfpop_data)
}
