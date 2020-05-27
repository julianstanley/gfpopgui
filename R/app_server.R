#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import plotly
#' @import gfpop
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  # Home page-------------------------------------------------------------------
  gfpop_data <- reactiveValues()

  # TODO: main_data should also accept .Rdata from `input$completed_analysis`
  output$main_data <- renderDataTable(
    {
      if (isTruthy(input$primary_input)) {
        primary_input <- read.csv(input$primary_input$datapath, header = F)
        colnames(primary_input) <- c("X", "Y")
        gfpop_data$primary_input <- primary_input
        primary_input
      } else if (isTruthy(gfpop_data$primary_input)) {
        gfpop_data$primary_input
      }
      else {
        validate(FALSE, "No input data provided. Please provide an input file.")
      }
    },
    options = list("pageLength" = 5, dom = "tp", searching = F)
  )

  # TODO: main_data should also accept .Rdata from `input$completed_analysis`
  output$graph <- renderDataTable(
    {
      if (isTruthy(input$constraint_graph)) {
        graph_input <- read.csv(input$constraint_graph$datapath, header = F)
        gfpop_data$graph_input <- graph_input
        graph_input
      } else {
        validate(FALSE, "No graph data provided. 
Graph data is optional. Please either provide a graph input file or move to the 'analysis' page")
      }
    },
    options = list("pageLength" = 5, dom = "tp", searching = F)
  )

  # Generate some data if users ask for it (temporary, for testing)
  observeEvent(input$genData, {
    primary_input <- data.frame(
      X = 1:input$ndata,
      Y = dataGenerator(input$ndata, c(0.1, 0.3, 0.5, 0.8, 1), c(1, 2, 1, 3, 1), sigma = input$sigma)
    )
    gfpop_data$primary_input <- primary_input
  })

  # Analysis page---------------------------------------------------------------

  # Generate a graph with the current penalty
  graphdf <- reactive({
    golem::cat_dev("graphdf\n")
    gfpop::graph(penalty = as.double(input$pen), type = input$graphType)
  })

  graphdf_visNetwork <- reactive({
    golem::cat_dev("graphdf_visNetwork\n")
    graphdf_normal <- graphdf() %>%
      filter(type != "null")
    edge_names <- paste0(graphdf_normal$state1, graphdf_normal$state2)
    node_names <- unique(c(graphdf_normal$state1, graphdf_normal$state2))
    list(
      nodes = data.frame(id = node_names, label = node_names),
      edges = data.frame(
        id = edge_names, label = edge_names,
        to = graphdf_normal$state2, from = graphdf_normal$state1
      )
    )
  })


  init.nodes <- data.frame(
    id = c("up", "down"),
    label = c("up", "down")
  )
  init.edges <- data.frame(
    id = c("updown", "downup", "upup"),
    to = c("up", "down", "up"),
    from = c("down", "up", "up"),
    label = c("updown", "downup", "upup")
  )

  # Generate a changepoint df from the current graph
  changepointdf <- reactive({
    req(gfpop_data$primary_input)
    generate_changepoint(gfpop_data$primary_input$Y, graphdf())
  })

  # visualize the network, not currently editable
  output$gfpopGraph <- renderVisNetwork({
    golem::cat_dev("output-gfpopGraph\n")
    graph_data <- graphdf_visNetwork()
    print(graph_data)
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visEdges(
        arrows = "to", physics = FALSE,
        smooth = list(
          type = "curvedCW",
          roundness = 0.2
        )
      ) %>%
      visOptions(manipulation = list(
        enabled = TRUE,
        editEdge = TRUE
      ))
  })

  # Main gfpop plot with red lines
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

    g <-
      ggplot(changepoint_data_annot, aes(
        x = X,
        y = Y,
        text = CP_Data
      )) +
      geom_point() +
      geom_segment(
        aes(
          x = changepoint,
          xend = changepoint_end,
          y = y,
          yend = y
        ),
        size = 1.5,
        col = "red"
      ) +
      xlab("X units (arbitrary)") +
      ylab("Univariate gaussian data (randomly generated)")
    ggplotly(g, tooltip = c("X", "Y", "text"))
  })
}
