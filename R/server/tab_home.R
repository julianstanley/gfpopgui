gfpop_data <- reactiveValues()

get_main_data <- reactive({
  if (isTruthy(input$primary_input)) {
    primary_input <- fread(input$primary_input$datapath, header = F, stringsAsFactors = FALSE)
    colnames(primary_input) <- c("X", "Y")
    gfpop_data$primary_input <- primary_input
    return(primary_input)
  } else if (isTruthy(gfpop_data$primary_input)) {
    return(gfpop_data$primary_input)
  }
  else {
    validate(FALSE, "No input data provided. Please provide an input file.")
  }
})

# TODO: main_data should also accept .Rdata from `input$completed_analysis`
# Parse the primary input data and return back a DataTable
output$main_datatable <- renderDataTable(
  {
    get_main_data()
  },
  options = list("pageLength" = 5, dom = "tp", searching = F)
)

# TODO: main_data should also accept .Rdata from `input$completed_analysis`
output$graph <- renderDataTable(
  {
    if (isTruthy(input$constraint_graph)) {
      graph_input <- fread(input$constraint_graph$datapath, stringsAsFactors = FALSE)
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