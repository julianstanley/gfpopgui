#' @description A module corresponding to the main "Analysis" tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton HTML NS numericInput selectInput
#' tags fluidRow column h2 br hr h5 checkboxInput tabsetPanel verbatimTextOutput
#' textInput uiOutput htmlOutput
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
        actionButton(inputId = ns("clsCp"), label = "Clear Changepoints"),
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
      column(
        2,
        hr(),
        h2("Save"),
        textInput(
          inputId = ns("saveId"),
          label = "Unique Save Name"
        ),
        actionButton(
          inputId = ns("saveButton"),
          label = "Save Analysis"
        ),
        h2("Load"),
        uiOutput(ns("uiLoadId")),
        actionButton(
          inputId = ns("loadButton"),
          label = "Load Analysis"
        )
      ),
      column(
        5,
        h2("Graph Details", align = "center"),
        tabsetPanel(
          tabPanel(
            "View/Edit",
            h3("Current edges and nodes (double click to edit)"),
            hr(),
            h5("Edges"),
            dataTableOutput(ns("graphOutput_visEdges")),
            h5("Nodes"),
            dataTableOutput(ns("graphOutput_visNodes")),

            h3("More graph options"),
            hr(),
            details(
              summary = "Generate a default graph", summary_multiplier = 1.5,
              content = tagList(
                inline_div(0.4, numericInput(
                  inputId = ns("pen"),
                  label = "Penalty",
                  value = 15
                )),
                inline_div(0.4, selectInput(
                  inputId = ns("graphType"),
                  label = "Graph Type",
                  choices = c("std", "isotonic", "updown", "relevant")
                )),
                br(),
                # Generate graph
                actionButton(inputId = ns("updateGraph"), label = "Update Graph")
              )
            ),

            # Start and end
            details(
              summary = "Set start and end nodes", summary_multiplier = 1.5,
              content = tagList(
                inline_div(0.4, uiOutput(ns("uiSetStart"))),
                inline_div(0.4, uiOutput(ns("uiSetEnd"))),
                actionButton(inputId = ns("setStartEnd_button"), label = "Apply Start/End Changes"),
              )
            ),
            details(
              summary = "Add/remove nodes", summary_multiplier = 1.5,
              content = tagList(
                tabsetPanel(
                  tabPanel(
                    "Add",
                    br(),
                    inline_div(0.4, textInput(
                      inputId = ns("addNode_id"),
                      label = "Provide a unique ID"
                    )),
                    br(),
                    actionButton(
                      inputId = ns("addNode_button"),
                      label = "Add New Node"
                    )
                  ),
                  tabPanel(
                    "Remove",
                    br(),
                    inline_div(0.4, uiOutput(ns("ui_setRemoveNode"))),
                    br(),
                    actionButton(
                      inputId = ns("removeNode_button"),
                      label = "Remove Node"
                    )
                  )
                )
              )
            ),
            details(
              summary = "Add/remove edges", summary_multiplier = 1.5,
              content = tagList(
                tabsetPanel(
                  tabPanel(
                    "Add",
                    br(),
                    inline_div(0.3, textInput(
                      inputId = ns("addEdge_id"),
                      label = "Provide a unique ID"
                    )),
                    inline_div(0.3, uiOutput(ns("ui_addEdge_from"))),
                    inline_div(0.3, uiOutput(ns("ui_addEdge_to"))),
                    br(),
                    inline_div(0.3, selectInput(
                      inputId = ns("addEdge_type"),
                      label = "Select edge type",
                      choices = c(
                        "std", "null",
                        "up", "down", "abs"
                      )
                    )),
                    inline_div(0.3, numericInput(
                      inputId = ns("addEdge_parameter"),
                      label = "Parameter (e.g. gap)",
                      value = 0, step = 0.5
                    )),
                    inline_div(0.3, numericInput(
                      inputId = ns("addEdge_penalty"),
                      label = "Penalty",
                      value = 15, step = 1
                    )),

                    actionButton(
                      inputId = ns("addEdge_button"),
                      label = "Add Edge"
                    )
                  ),
                  tabPanel(
                    "Remove",
                    br(),
                    inline_div(0.4, uiOutput(ns("ui_setRemoveEdge"))),
                    br(),
                    actionButton(
                      inputId = ns("removeEdge_button"),
                      label = "Remove Edge"
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "GFPOP Graph",
            h3("Current Graph"),
            h5("This is what gets sent to gfpop"),
            dataTableOutput(ns("graphOutput")),
            uiOutput(ns("graphOutput_code"))
          ),
          tabPanel(
            "Help",
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
    
    # Test
    verbatimTextOutput(ns("click"))
  )
}

#' analysis Server Function
#'
#' @noRd
#' @importFrom shiny reactiveValues observeEvent req reactive isTruthy validate
#' isolate updateNumericInput renderUI renderText
#' @importFrom plotly ggplotly renderPlotly plot_ly add_markers
#' @importFrom visNetwork renderVisNetwork visNetworkProxy visUpdateNodes visUpdateEdges
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom data.table data.table
#' @importFrom dplyr mutate filter
#' @importFrom gfpop gfpop
#' @importFrom rlang .data
#' @importFrom shinyalert shinyalert
#' @importFrom plyr rbind.fill
#' @importFrom shinyjs onevent
#' @import gfpop
#' @export
mod_analysis_server <- function(id, gfpop_data = reactiveValues()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## Saving and Loading ----------------------------------------------------
      ## Keep track of some analyses
      saved_analyses <- reactiveValues(
        saved_full = list(),
        saved_descriptions = data.table()
      )

      startEnd <- reactiveValues(
        start = "N/A",
        end = "N/A"
      )
      
      selected <- reactiveValues(
        nodes = c(),
        edges = c()
      )

      ## Render the saved analyses
      output$uiLoadId <- renderUI({
        selectInput(ns("loadId"), "Select a previous analysis",
          choices = saved_analyses$saved_descriptions$id
        )
      })

      # Observe a save
      observeEvent(input$saveButton, {
        req(input$saveId)
        saveId <- input$saveId

        if (saveId %in% names(saved_analyses$saved_full)) {
          shinyalert(
            title = "Duplicate ID",
            text = "Save IDs must be unique. Please provide a different ID.",
            type = "error"
          )
        } else {
          saved_analyses$saved_full[[saveId]] <- reactiveValuesToList(gfpop_data)
          saved_analyses$saved_descriptions <- rbind(
            saved_analyses$saved_descriptions,
            data.table(id = input$saveId)
          )
        }

        updateTextInput(session, "saveId", value = "")
      })

      # Observe a load
      observeEvent(input$loadButton, {
        req(input$loadId)
        gfpop_data <<- do.call("reactiveValues", saved_analyses$saved_full[[input$loadId]])

        # Update the graph
        updateNumericInput(
          session = session, inputId = "graph_refresh_helper",
          value = input$graph_refresh_helper + 1
        )
      })

      ## Graph Logistics -------------------------------------------------------

      # A reactive value to trigger a graph refresh
      dummy_graph_refresh <- reactiveValues(i = 0)

      # When the "Update graph with above parameters" button is pressed, update graph
      # Note: for testing purposes, all observeEvent's are
      observeEvent(eventExpr = input$updateGraph, {
        gfpop_data$graphdata <- gfpop::graph(
          penalty = as.double(input$pen),
          type = input$graphType
        )

        gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
          gfpop_data$graphdata,
          showNull = input$showNull
        )

        # When we update the graph this way, we should do a full refresh
        dummy_graph_refresh$i <- dummy_graph_refresh$i + 1
      })

      # Toggle whether null edges are visible
      observeEvent(eventExpr = input$showNull, {
        gfpop_data$graphdata_visNetwork$edges$hidden <- sapply(
          gfpop_data$graphdata_visNetwork$edges$type,
          function(x) if (input$showNull) FALSE else (x == "null")
        )
      })

      # Update visNetwork graph in-place (via visNetworkProxy)
      observe({
        # Update edge labels
        gfpop_data$graphdata_visNetwork$edges$label <- create_label(
          gfpop_data$graphdata_visNetwork$edges
        )

        # Update graph edges and nodes
        visNetworkProxy(ns("gfpopGraph")) %>%
          visUpdateNodes(nodes = gfpop_data$graphdata_visNetwork$nodes) %>%
          visUpdateEdges(edges = gfpop_data$graphdata_visNetwork$edges)
      })

      # Generate a visualization of the current constraint graph
      output$gfpopGraph <- renderVisNetwork({
        # Refresh when the refresh Graph button is pressed
        # Or when the dummy graph refresh is modified
        input$refreshGraph
        dummy_graph_refresh$i

        generate_visNetwork(isolate(gfpop_data$graphdata_visNetwork))
      })

      ### Graph Logistics: Observe graph changes-------------------------------------

      # Respond to a change in the visNetwork plot (via manipulation params)
      observeEvent(input$gfpopGraph_graphChange, {
        event <- input$gfpopGraph_graphChange

        gfpop_data$graphdata_visNetwork <- modify_visNetwork(
          event,
          gfpop_data$graphdata_visNetwork
        )

        # We also need to make sure graphdata stays in sync with visNetwork
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      ### Graph Logistics: editing under "View/Edit" ---------------------------

      # UI Components
      output$uiSetStart <- renderUI({
        selectInput(ns("setStart"), "Select a starting node",
          choices = c(
            "N/A",
            gfpop_data$graphdata_visNetwork$nodes$label
          ),
          selected = startEnd$start
        )
      })

      output$uiSetEnd <- renderUI({
        selectInput(ns("setEnd"), "Select an ending node",
          choices = c(
            "N/A",
            gfpop_data$graphdata_visNetwork$nodes$label
          ),
          selected = startEnd$end
        )
      })

      output$ui_setRemoveNode <- renderUI({
        selectInput(ns("setRemoveNode"), "Select a node to remove",
          choices = c(gfpop_data$graphdata_visNetwork$nodes$id)
        )
      })

      output$ui_setRemoveEdge <- renderUI({
        selectInput(ns("setRemoveEdge"), "Select an edge to remove",
          choices = c(gfpop_data$graphdata_visNetwork$edges$id)
        )
      })

      output$ui_addEdge_to <- renderUI({
        selectInput(ns("addEdge_to"), "To which node?",
          choices = c(gfpop_data$graphdata_visNetwork$nodes$id)
        )
      })

      output$ui_addEdge_from <- renderUI({
        selectInput(ns("addEdge_from"), "From which node?",
          choices = c(gfpop_data$graphdata_visNetwork$nodes$id)
        )
      })

      # Deal with adding a start node
      observeEvent(input$setStartEnd_button, {

        # new_val: the new start or new end
        # val_type: "start" or "end"
        set_startEnd <- function(new_val, val_type) {
          if (new_val != "N/A") {
            gfpop_data$graphdata <<- gfpop::graph(
              gfpop_data$graphdata %>%
                rbind.fill(data.frame(state1 = new_val, type = val_type))
            )
          } else {
            gfpop_data$graphdata <<- gfpop::graph(
              data.frame(gfpop_data$graphdata) %>%
                filter(type != val_type)
            )
          }
        }

        set_startEnd(input$setStart, "start")
        set_startEnd(input$setEnd, "end")

        # Set these so that the "start" and "end" dropdown boxes, which are
        # refreshed when graphdata updates, knows about the current start & end
        startEnd$start <- input$setStart
        startEnd$end <- input$setEnd

        # Update the visNetwork data to match the gfpop data
        gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
          gfpop_data$graphdata
        )
      })

      # Deal with adding nodes
      observeEvent(input$addNode_button, {
        if (input$addNode_id %notin% gfpop_data$graphdata_visNetwork$nodes$id) {
          gfpop_data$graphdata_visNetwork$nodes <- add_node(
            gfpop_data$graphdata_visNetwork$nodes,
            id = input$addNode_id,
            label = input$addNode_id
          )
        } else {
          shinyalert(
            title = "Duplicate ID",
            text = "Node IDs must be unique. Please provide
                                 a unique node ID", type = "error"
          )
        }
      })

      # Deal with removing nodes
      observeEvent(input$removeNode_button, {
        gfpop_data$graphdata_visNetwork$nodes <- gfpop_data$graphdata_visNetwork$nodes %>%
          filter(id != input$setRemoveNode)
        gfpop_data$graphdata_visNetwork$edges <- gfpop_data$graphdata_visNetwork$edges %>%
          filter(to != input$setRemoveNode & from != input$setRemoveNode)

        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      # Deal with adding edges
      observeEvent(input$addEdge_button, {
        gfpop_data$graphdata_visNetwork$edges <- gfpop_data$graphdata_visNetwork$edges %>%
          rbind.fill(data.frame(
            id = input$addEdge_id, to = input$addEdge_to, from = input$addEdge_from,
            type = input$addEdge_type, parameter = input$addEdge_parameter,
            penalty = input$addEdge_penalty
          ))
        gfpop_data$graphdata_visNetwork$edges$label <- gfpop_data$graphdata_visNetwork$edges %>%
          create_label()
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })





      ### Graph Logistics: Output Data Tables & Code-----
      output$graphOutput_code <- renderUI({
        details("Current Graph (R Code)", paste0(
          "<code>",
          gsub(
            " ", "&nbsp;",
            gsub(pattern = "\n", "<br>", graph_to_R_code(gfpop_data$graphdata %>%
                                                           select_graph_columns()))
          ),
          "<br><br>",
          "</code>"
        ),
        summary_multiplier = 1.5
        )
      })

      output$graphOutput <- DT::renderDT(
        {
          gfpop_data$graphdata %>% select_graph_columns()
        },
        editable = TRUE,
        options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
      )

      output$graphOutput_visEdges <- DT::renderDT(
        {
          gfpop_data$graphdata_visNetwork$edges
        },
        editable = TRUE,
        options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
      )

      output$graphOutput_visNodes <- DT::renderDT(
        {
          gfpop_data$graphdata_visNetwork$nodes
        },
        editable = TRUE,
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

      # Clear changepoints
      observeEvent(input$clsCp, {
        gfpop_data$changepoints <- NULL
        gfpop_data$changepoint_plot <- gfpop_data$base_plot
      })

      # From the current input data and graph, generate changepoint results
      # Returns: None. Affects: initializes gfpop_data$changepoints
      initialize_changepoints <- reactive({
        req(gfpop_data$main_data)

        # Update this function when changepoints update
        gfpop_data$changepoints

        tryCatch(
          expr = {
            # TODO: Allow user to add weights (what do those do?)
            gfpop_data$changepoints <<- gfpop::gfpop(gfpop_data$main_data$Y,
              gfpop_data$graphdata %>% select_graph_columns(),
              type = input$gfpopType
            )
          },
          error = function(e) {
            shinyalert(
              title = "Failed to initalize changepoints",
              text = e, type = "error"
            )
          },
          warning = function(w) {
            shinyalert(
              title = "Changepoint warning",
              w, type = "warning"
            )
          }
        )
      })

      # Creates a base plotly plot with the user data
      # Returns: None. Affects: initializes gfpop_data$base_plot
      initialize_plot <- observeEvent(gfpop_data$main_data, {
        req(gfpop_data$main_data)

        gfpop_data$base_plot <-
          plot_ly(gfpop_data$main_data, x = ~X, y = ~Y, hoverinfo = "none", source = "gfpopPlot") %>%
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
        input$loadButton

        if (isTruthy(gfpop_data$changepoints)) {
          gfpop_data$changepoint_plot
        } else {
          gfpop_data$base_plot
        }
      })

      output$gfpopOutput <- DT::renderDT(
        {
          input$loadButton
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
        input$loadButton
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
      
      hover_data <- reactive({
        req(gfpop_data$base_plot)
        event_data("plotly_hover", "gfpopPlot")
      })
      
      observeEvent(hover_data(), {
        event <- hover_data()
        nevents <- dim(event)[1]
        if(nevents > 1) {
          selection <- event$key[2]
          # If the current selection is different than the last one
          if(event$key[2] %notin% selected$nodes) {
          # We are selecting a new thing! First, unselect anything that is 
          # selected
          if(length(selected$nodes) > 0) {
            visNetworkProxy(ns("gfpopGraph")) %>%
              visUpdateNodes(
                gfpop_data$graphdata_visNetwork$nodes %>%
                  mutate(color.border = "lightblue", color.border = "lightblue",
                         shadow = FALSE)
              )
            selected$nodes <- c()
          }
          
          # Now, make a selection!
          
          visNetworkProxy(ns("gfpopGraph")) %>%
            visUpdateNodes(
              gfpop_data$graphdata_visNetwork$nodes %>%
                filter(label == selection) %>%
                mutate(color.background = "#D2E5FF", color.border = "#3F89EC",
                       shadow = TRUE)
            )
          selected$nodes <- c(selection)
          }
    
        } else if(nevents > 2) {
          # TODO: Allow for edge highlighting
        }
      })
      
      onevent("mouseleave", "gfpopPlot", {
        if(length(selected$nodes) > 0) {
          visNetworkProxy(ns("gfpopGraph")) %>%
            visUpdateNodes(
              gfpop_data$graphdata_visNetwork$nodes 
            )
        }
      })
    }
  )

  gfpop_data
}
