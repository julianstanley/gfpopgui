#' analysis UI Function
#'
#' @description A module corresponding to the main "Analysis" tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton HTML NS numericInput selectInput
#' tags fluidRow column h2 br hr h5 checkboxInput tabsetPanel htmlOutput
#' @import shinycssloaders
#' @importFrom plotly plotlyOutput
#' @importFrom visNetwork visNetworkOutput
#' @importFrom DT dataTableOutput
#' @importFrom utils str
#' @importFrom shinyjs hidden
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
          ),
          tabPanel(
            "More",
            h3("Full Output"),
            htmlOutput(ns("gfpopOutput_verbose")))
        )
      )
    ),

    # Buttons for debugging
    actionButton(ns("browser"), "browser"),
    tags$script(paste0("$('#", ns("browser"), "').hide();")),
    hidden(numericInput(inputId = ns("graph_refresh_helper"), label = "", value = 0))
  )
}

#' analysis Server Function
#'
#' @noRd
#' @importFrom shiny reactiveValues observeEvent req reactive isTruthy validate
#' isolate updateNumericInput
#' @importFrom plotly ggplotly renderPlotly plot_ly add_markers 
#' @importFrom visNetwork renderVisNetwork
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom dplyr mutate filter
#' @importFrom gfpop gfpop
#' @importFrom rlang .data
#' @import gfpop
#' @export
mod_analysis_server <- function(id, gfpop_data = reactiveValues()) {
  moduleServer(
    id, 
    function(input, output, session) {
      ## Graph Logistics -----------------------------------------------------------
      
      # Observer to see the graph refresh helper
      observeEvent( input$graph_refresh_helper, {} )
      
      # When the "Update graph with above parameters" button is pressed, update graph
      updateGraph <- reactive({
        gfpop_data$graphdata <- gfpop::graph(
          penalty = as.double(isolate(input$pen)),
          type = isolate(input$graphType)
        )
        gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
          gfpop_data$graphdata,
          showNull = input$showNull
        )
      })
      
      observeEvent(eventExpr = input$updateGraph, {
        updateGraph()
      })
      
      # Hide null nodes
      hideNull <- reactive({
        gfpop_data$graphdata_visNetwork$edges$hidden <- sapply(
          gfpop_data$graphdata_visNetwork$edges$type,
          function(x) if (input$showNull) FALSE else (x == "null")
        )
      })
      
      observeEvent(eventExpr = input$showNull, {
        hideNull()
      })
      
      # Generate a visualization of the current constraint graph
      output$gfpopGraph <- renderVisNetwork({
        # Refresh when one of the Update/Refresh Graph buttons are pressed
        input$updateGraph
        input$refreshGraph
        
        # Or, use this to trigger from elsewhere
        input$graph_refresh_helper
        
        generate_visNetwork(isolate(gfpop_data$graphdata_visNetwork))
      })
      
      ### Graph Logistics: Observe graph changes-------------------------------------
      # Monitor edge edits. Edit the gfpop_data$graphdata_visNetwork variable
      # from the observations, and then update the gfpop_data$graphdata
      observeEvent(input$gfpopGraph_graphChange, {
        event <- input$gfpopGraph_graphChange
        
        ### Edit Edge --------------------------------------------------------------
        if (event$cmd == "editEdge") {
          
          changed_id <- event$id
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
          
          # Need to refresh graph for things to work properly here
          updateNumericInput(session = session, inputId = 'graph_refresh_helper', 
                             value = input$graph_refresh_helper + 1)
        }
        
        ### Add Edge ---------------------------------------------------------------
        if (event$cmd == "addEdge") {
          new_row <- data.frame(
            id = event$id,
            label = "",
            to = event$to, from = event$from,
            type = "null", parameter = "0",
            penalty = "0", K = "Inf", a = "0",
            min = "None", max = "None", 
            selfReference.angle = NA, selfReference.size = 40, hidden = FALSE)
          
          gfpop_data$graphdata_visNetwork$edges <- rbind(gfpop_data$graphdata_visNetwork$edges,
                                                         new_row)
          
          # Need to refresh graph for things to work properly here
          updateNumericInput(session = session, inputId = 'graph_refresh_helper', 
                             value = input$graph_refresh_helper + 1)
          
        }
        
        ### Delete Edge ------------------------------------------------------------
        if (event$cmd == "deleteElements" && (length(event$edges) > 0)) {
          print(length(event$edges))
          for(del_edge in event$edges) {
            gfpop_data$graphdata_visNetwork$edges <-
              gfpop_data$graphdata_visNetwork$edges %>%
              dplyr::filter(.data$id != del_edge)
          }
        }
        
        ### Add Node ---------------------------------------------------------------
        if (event$cmd == "addNode") {
          gfpop_data$graphdata_visNetwork$nodes <- rbind(
            gfpop_data$graphdata_visNetwork$nodes,
            data.frame(id = event$id,
                       label = event$label,
                       size = 40))
        }
        
        ### Edit Node --------------------------------------------------------------
        if (event$cmd == "editNode") {
          gfpop_data$graphdata_visNetwork$nodes <- 
            gfpop_data$graphdata_visNetwork$nodes %>%
            mutate_cond(id == event$id,
                        label = event$label)
        }
        
        ### Delete Node ------------------------------------------------------------
        if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
          for(del_node in event$nodes) {
            gfpop_data$graphdata_visNetwork$nodes <-
              gfpop_data$graphdata_visNetwork$nodes %>%
              dplyr::filter(.data$id != del_node)
          }
        }
        
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })
      
      ### Graph Logistics: Output Data Tables -----
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
      
      ### Graph Logistics: Edit Observers for Output Data Tables -----
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
        gfpop_data$base_plot %>%
          add_changepoints(gfpop_data$main_data, gfpop_data$changepoints)
        
      })
      
      output$gfpopOutput <- DT::renderDT(
        {
          changepoints <- gfpop_data$changepoints
          data.frame(
            "State" = changepoints$states,
            "X Location" = changepoints$changepoints,
            "Y Mean Before CP" = changepoints$parameters
          )
        },
        editable = FALSE,
        options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
      )
      
      output$gfpopOutput_verbose <- renderUI({
        changepoints <- gfpop_data$changepoints
        outputstr <- paste(
          "<b>Changepoints:</b>",
          paste(changepoints$changepoints, collapse = ","), 
          "<b>States:</b>",
          paste(changepoints$states, collapse = ","), 
          "<b>Forced:</b>",
          paste(changepoints$forced, collapse = ","), 
          "<b>Parameters (means):</b>", 
          paste(round(changepoints$parameters, 2), collapse = ","),
          "<b>Global Cost:</b>",
          round(changepoints$globalCost, 2),
          sep = '<br/>'
        )
        
        HTML(outputstr)
      })
      
      # For debugging --------------------------------------------------------------
      observeEvent(input$browser, {
        browser()
      })
    }
  )
}
