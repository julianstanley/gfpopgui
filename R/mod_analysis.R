#' analysis UI Function
#'
#' @description A module corresponding to the main "Analysis" tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton HTML NS numericInput selectInput
#' tags fluidRow column h2 br hr h5 checkboxInput tabsetPanel
#' @import shinycssloaders
#' @importFrom plotly plotlyOutput
#' @importFrom visNetwork visNetworkOutput
#' @importFrom DT dataTableOutput
#' @importFrom utils str
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2,
        h2("Settings"),
        hr(),

        h4("global graph parameters"),

        numericInput(
          inputId = ns("pen"),
          label = "Penalty",
          value = 15
        ),
        selectInput(
          inputId = ns("graphType"),
          label = "Graph Type",
          choices = c("std", "isotonic", "updown", "relevant")
        ),

        # Generate graph
        actionButton(inputId = ns("updateGraph"), label = "Update Graph"),
        hr(),

        h4("other graph settings"),
        checkboxInput(
          inputId = ns("showNull"),
          label = "Show null nodes?",
          value = TRUE
        ),
        actionButton(inputId = ns("refreshGraph"), label = "Refresh Graph"),
        hr(),
        h4("gfpop settings"),
        selectInput(
          inputId = ns("gfpopType"),
          label = "Cost model",
          choices = c("mean", "variance", "poisson", "exp", "negbin")
        ),

        actionButton(inputId = ns("runGfpop"), label = "Run gfpop!"),
        hr(),
        HTML("<b>Note:</b> Edge and Node IDs <u>cannot</u> be changed once
             they are created. Please set IDs appropriately (or accept the defaults)
             when you create them!"),
      ),
      column(
        5,
        h2("Graph", align = "center"),
        hr(),
        visNetworkOutput(ns("gfpopGraph")) %>% withSpinner(type = 6)
      ),
      column(
        5,
        h2("Changepoints", align = "center"),
        hr(),
        plotlyOutput(ns("gfpopPlot")) %>% withSpinner(type = 6)
      )
    ),
    # Row with datatable outputs
    fluidRow(
      column(2),
      column(
        5,
        h2("Graph (Editable DataTable)", align = "center"),
        tabsetPanel(
          tabPanel(
            "Basic",
            h3("Current Graph"),
            h5("This is what gets sent to gfpop"),
            dataTableOutput(ns("graphOutput"))
          ),
          tabPanel(
            "Advanced",
            h3("Edges"),
            dataTableOutput(ns("graphOutput_visEdges")),
            h3("Nodes"),
            dataTableOutput(ns("graphOutput_visNodes"))
          )
        )
      ),
      column(
        5,
        h2("gfpop output"),
        tabsetPanel(
          tabPanel(
            "Main",
            h3("Changepoints"),
            dataTableOutput(ns("gfpopOutput"))
          )
        )
      )
    ),

    # Button for debugging
    actionButton(ns("browser"), "browser"),
    tags$script(paste0("$('#", ns("browser"), "').hide();"))
  )
}

#' analysis Server Function
#'
#' @noRd
#' @importFrom shiny reactiveValues observeEvent req reactive isTruthy validate
#' isolate
#' @importFrom plotly ggplotly renderPlotly plot_ly add_markers
#' @importFrom visNetwork renderVisNetwork
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom dplyr mutate
#' @importFrom gfpop gfpop
#' @import gfpop
mod_analysis_server <- function(input, output, session, gfpop_data) {
  ns <- session$ns

  ## Graph Logistics -----------------------------------------------------------

  # When the "Update graph with above parameters" button is pressed, update graph
  graphdf_default <- observeEvent(eventExpr = input$updateGraph, {
    gfpop_data$graphdata <- gfpop::graph(
      penalty = as.double(isolate(input$pen)),
      type = isolate(input$graphType)
    )
    gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
      gfpop_data$graphdata,
      showNull = input$showNull
    )
  })

  # Hide nodes
  observeEvent(eventExpr = input$showNull, {
    gfpop_data$graphdata_visNetwork$edges$hidden <- sapply(
      gfpop_data$graphdata_visNetwork$edges$type,
      function(x) if (input$showNull) FALSE else (x == "null")
    )
  })

  # Generate a visualization of the current constraint graph
  output$gfpopGraph <- renderVisNetwork({
    # Refresh when one of the Update/Refresh Graph buttons are pressed
    input$updateGraph
    input$refreshGraph
    generate_visNetwork(isolate(gfpop_data$graphdata_visNetwork))
  })

  ## Graph Logistics: Monitoring edits -----------------------------------------
  # Monitor edge edits. Edit the gfpop_data$graphdata_visNetwork variable
  # from the observations, and then update the gfpop_data$graphdata
  observeEvent(input$gfpopGraph_graphChange, {
    event <- input$gfpopGraph_graphChange
    changed_id <- event$id
    
    if (event$cmd == "editEdge") {
      
      # Decide whether we need to add selfReference.angle
      if (event$to == event$from) {
        angle <- if (event$type == "null") pi else 2*pi 
      } else {
        angle = "NA"
      }

      gfpop_data$graphdata_visNetwork$edges <- gfpop_data$graphdata_visNetwork$edges %>%
        mutate_cond(id == changed_id,
          label = paste0(event$type, " | ", event$penalty),
          to = event$to, from = event$from,
          type = event$type, parameter = event$parameter,
          penalty = event$penalty, K = event$K, a = event$a,
          min = event$min, max = event$max,
          selfReference.angle = angle, selfReference.size = 40,
          hidden = as.logical(event$hidden)
        )
    }
    
    if (event$cmd == "addEdge") {
      new_row <- data.frame(
        id = event$id,
        label = "",
        to = event$to, from = event$from,
        type = "null", parameter = "None",
        penalty = "None", K = "None", a = "None",
        min = "None", max = "None", 
        selfReference.angle = NA, selfReference.size = NA, hidden = FALSE)
      
      gfpop_data$graphdata_visNetwork$edges <- rbind(gfpop_data$graphdata_visNetwork$edges,
                                                     new_row)
    }
    
    if (event$cmd == "addNode") {
      gfpop_data$graphdata_visNetwork$nodes <- rbind(
        gfpop_data$graphdata_visNetwork$nodes,
                             data.frame(id = event$id,
                                        label = event$label,
                                        size = 40))
    }
    
    if (event$cmd == "editNode") {
      gfpop_data$graphdata_visNetwork$nodes <- 
        gfpop_data$graphdata_visNetwork$nodes %>%
        mutate_cond(id == event$id,
                    label = event$label)
    }

    gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
  })

  ## Graph Logistics: Output Data Tables -----
  output$graphOutput <- DT::renderDT(
    {
      gfpop_data$graphdata
    },
    editable = TRUE,
    selection = "none",
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  output$graphOutput_visEdges <- DT::renderDT(
    {
      gfpop_data$graphdata_visNetwork$edges
    },
    editable = TRUE,
    selection = "none",
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  output$graphOutput_visNodes <- DT::renderDT(
    {
      gfpop_data$graphdata_visNetwork$nodes
    },
    editable = TRUE,
    selection = "none",
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  # Graph Logistics: Edit Observers for Output Data Tables -----
  proxy <- dataTableProxy("graphOutput")
  observeEvent(input$graphOutput_cell_edit, {
    info <- input$graphOutput_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    gfpop_data$graphdata[i, j] <<- DT::coerceValue(v, gfpop_data$graphdata[i, j])
    replaceData(proxy, gfpop_data$graphdata, resetPaging = FALSE)
    gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata, showNull = input$showNull)
  })

  proxy_visEdges <- dataTableProxy("graphOutput_visEdges")
  observeEvent(input$graphOutput_visEdges_cell_edit, {
    info <- input$graphOutput_visEdges_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    gfpop_data$graphdata_visNetwork$edges[i, j] <<- DT::coerceValue(
      v, gfpop_data$graphdata_visNetwork$edges[i, j]
    )
    replaceData(proxy_visEdges, gfpop_data$graphdata_visNetwork$edges, resetPaging = FALSE)
    gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
  })

  proxy_visNodes <- dataTableProxy("graphOutput_visNodes")
  observeEvent(input$graphOutput_visNodes_cell_edit, {
    info <- input$graphOutput_visNodes_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    gfpop_data$graphdata_visNetwork$nodes[i, j] <<- DT::coerceValue(
      v, gfpop_data$graphdata_visNetwork$nodes[i, j]
    )
    replaceData(proxy_visNodes, gfpop_data$graphdata_visNetwork$nodes, resetPaging = FALSE)
    gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
  })

  ## Changepoint/data logistics ------------------------------------------------

  # From the current input data and graph, generate changepoint results
  # Returns: None. Affects: initializes gfpop_data$changepoints
  initialize_changepoints <- reactive({
    req(gfpop_data$main_data)
    # TODO: Allow user to add weights (what do those do?)
    gfpop_data$changepoints <- gfpop::gfpop(gfpop_data$main_data$Y,
                                            gfpop_data$graphdata,
                                            type = input$gfpopType)
  })

  # Creates a base plotly plot with the user data
  # Returns: None. Affects: initializes gfpop_data$base_plot
  initialize_plot <- observeEvent(gfpop_data$main_data, {
    req(gfpop_data$main_data)
    
    gfpop_data$base_plot <- 
      plot_ly(gfpop_data$main_data, x = ~X, y = ~Y, hoverinfo = 'none') %>%
      add_markers
  })
  
  # Generate the visualization of the data with overlain changepoints
  output$gfpopPlot <- renderPlotly({
    # Triggered by button to run analysis
    input$runGfpop
    
    # Run the analysis
    isolate(initialize_changepoints())

    # Build on top of the current base data graphic
    gfpop_data$base_plot
    
  })

  output$gfpopOutput <- DT::renderDT(
    {
      gfpop_data$changepointdf
    },
    editable = FALSE,
    options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
  )

  # For debugging --------------------------------------------------------------
  observeEvent(input$browser, {
    browser()
  })
}
