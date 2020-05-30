# Initialize our main graphdf object
graphdf <- reactiveValues(graph = gfpop::graph(penalty = as.double(15), 
                                               type = "std"))

# When the "Update graph with above parameters" button is pressed,
# update the graph
graphdf_default <- observeEvent(eventExpr = input$updateGraph, {
  graphdf$graph <- gfpop::graph(penalty = as.double(isolate(input$pen)), 
                                  type = isolate(input$graphType))
})

# Some data wrangling: turn the graphdf into a format that visNetwork
# can understand
graphdf_visNetwork <- reactive({
  graphdf_normal <- graphdf$graph %>%
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

# From the current input data and graph, generate changepoint results
changepointdf <- reactive({
  req(gfpop_data$primary_input)
  generate_changepoint(gfpop_data$primary_input$Y, graphdf$graph, input$gfpopType)
})

update_graph <- reactive({
  if(isTruthy(input$constraint_graph)) {
    graphdf$graph = gfpop::graph(
      select(
        data.frame(fread(input$constraint_graph$datapath, stringsAsFactors = FALSE)),
        one_of(c("state1", "state2", "type", "parameter", "penalty", "K", "a", "min", "max")))
    )
  }
})
# Generate a visualization of the current constraint graph
# TODO: this isn't actually editable yet
output$gfpopGraph <- renderVisNetwork({
  # Only run with the "Run gfpop!" button is pressed
  input$runGfpop
  
  graph_data <- graphdf_visNetwork()
  visNetwork(graph_data$nodes, graph_data$edges) %>%
    visLayout(randomSeed = 123) %>%
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
  ggplotly(g, tooltip = c("X", "Y")) 
})