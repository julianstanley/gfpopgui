tabPanel(
  title = "Analysis",
  id= "analysis",
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
      
      # Generate graph
      actionButton(inputId = "updateGraph", label = "Update graph with above parameters"),
      HTML("<hr>"),
      
      # What cost model?
      selectInput( 
        inputId = "gfpopType",
        label = "Cost model",
        choices = c("mean", "variance", "poisson", "exp", "negbin")),
      
      # Submit!
      actionButton(inputId = "runGfpop", label = "Run gfpop!"),
      tags$br(),tags$br(),
      "Note: for testing, using type = mean"
    ),
    column(
      5,
      visNetworkOutput("gfpopGraph")
    ),
    column(
      5,
      plotlyOutput("gfpopPlot") %>% withSpinner(type = 6)
    )
  )
)