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
#' textInput uiOutput
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
             when you create them!")
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
      column(2,
            hr(),             
             h2("Save"),
             textInput(inputId = ns("saveId"),
                       label = "Unique Save Name"),
             actionButton(inputId = ns("saveButton"),
                          label = "Save Analysis"),
             h2("Load"),
             uiOutput(ns("uiLoadId")),
             actionButton(inputId = ns("loadButton"),
                          label = "Load Analysis")),
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
            htmlOutput(ns("gfpopOutput_verbose"))
          )
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
#' isolate updateNumericInput renderUI
#' @importFrom plotly ggplotly renderPlotly plot_ly add_markers
#' @importFrom visNetwork renderVisNetwork
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom data.table data.table
#' @importFrom dplyr mutate filter
#' @importFrom gfpop gfpop
#' @importFrom rlang .data
#' @importFrom shinyalert shinyalert
#' @import gfpop
#' @export
mod_analysis_server <- function(id, gfpop_data = reactiveValues()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Saving and Loading ----------------------------------------------------
      ## Keep track of some analyses
      saved_analyses <- reactiveValues(saved_full = list(), 
                                       saved_descriptions = data.table())
      
      ## Render the saved analyses
      output$uiLoadId <- renderUI({
        selectInput(ns("loadId"), "Select a previous analysis", 
                    choices = saved_analyses$saved_descriptions$id)
      })
      
      # Observe a save
      observeEvent(input$saveButton, {
        req(input$saveId)
        saveId <- input$saveId
        saved_analyses$saved_full[[saveId]] <- reactiveValuesToList(gfpop_data)
        saved_analyses$saved_descriptions <- rbind(saved_analyses$saved_descriptions,
                                                   data.table(id = input$saveId))
      })
      
      # Observe a load
      observeEvent(input$loadButton, {
        req(input$loadId)
        gfpop_data <<- do.call("reactiveValues", saved_analyses$saved_full[[input$loadId]])
      })

      ## Graph Logistics -------------------------------------------------------

      # Observer to see the graph refresh helper
      observeEvent(input$graph_refresh_helper, {})

      # When the "Update graph with above parameters" button is pressed, update graph
      updateGraph <- reactive({
        gfpop_data$graphdata <- gfpop::graph(
          penalty = as.double(input$pen),
          type = input$graphType
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

        modified_data <- modify_visNetwork(
          event,
          gfpop_data$graphdata_visNetwork
        )
        gfpop_data$graphdata_visNetwork <- modified_data$data
        if (modified_data$refresh) {
          updateNumericInput(
            session = session, inputId = "graph_refresh_helper",
            value = input$graph_refresh_helper + 1
          )
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
        
        tryCatch(
          expr = {
            # TODO: Allow user to add weights (what do those do?)
            gfpop_data$changepoints <- gfpop::gfpop(gfpop_data$main_data$Y,
                                                    gfpop_data$graphdata,
                                                    type = input$gfpopType
            )
          },
          error = function(e) {
            shinyalert(paste0(
              "Failed to initalize changepoints:\n ", e), type = "error")
          },
          warning = function(w) {
            shinyalert(paste0(
              "Got a warning while initalizing changepoints: ", w), type = "warning")
          }
        )
      })

      # Creates a base plotly plot with the user data
      # Returns: None. Affects: initializes gfpop_data$base_plot
      initialize_plot <- observeEvent(gfpop_data$main_data, {
        req(gfpop_data$main_data)

        gfpop_data$base_plot <-
          plot_ly(gfpop_data$main_data, x = ~X, y = ~Y, hoverinfo = "none") %>%
          add_markers()

        gfpop_data$changepoint_plot <- gfpop_data$base_plot
      })

      observeEvent(input$runGfpop, {
        initialize_changepoints()
        gfpop_data$changepoint_plot <- gfpop_data$base_plot %>%
          add_changepoints(
            isolate(gfpop_data$main_data),
            isolate(gfpop_data$changepoints)
          )
        
        
      })

      # Generate the visualization of the data with overlain changepoints
      output$gfpopPlot <- renderPlotly({
        gfpop_data$changepoint_plot
      })

      output$gfpopOutput <- DT::renderDT(
        {
          changepoints <- req(gfpop_data$changepoints)
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
        changepoints <- req(gfpop_data$changepoints)
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
          sep = "<br/>"
        )

        HTML(outputstr)
      })

      # For debugging --------------------------------------------------------------
      observeEvent(input$browser, {
        browser()
      })
    }
  )
  
  gfpop_data
}
