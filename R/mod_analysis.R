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
    # Top row (settings, graph, changepoints) ----------------------------------
    fluidRow(
      column(
        2,
        h2("Settings"),
        hr(),

        h4("other graph settings"),
        checkboxInput(
          inputId = ns("addNull"),
          label = "Automatically add recursive null edges?",
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("showNull"),
          label = "Show null edges?",
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("crosstalk"),
          label = "Enable Crosstalk?",
          value = TRUE
        ),
        selectInput(
          inputId = ns("labels"),
          label = "What info should edge labels contain?",
          choices = c(
            "state1", "state2", "type", "parameter", "penalty",
            "K", "a", "min", "max"
          ),
          selected = c("type", "parameter"),
          multiple = TRUE
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

      # Main graph and changepoints columns
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

    # Bottom row (graph settings, changepoint outputs) -------------------------
    fluidRow(

      # For saving and loading
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

      # For more constraint graph details
      column(
        5,
        h2("Graph Details", align = "center"),
        tabsetPanel(

          # For viewing and editing the constraint graph
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

          # For viewing the current graph and getting the associated R code
          tabPanel(
            "GFPOP Graph",
            h3("Current Graph"),
            h5("This is what gets sent to gfpop"),
            dataTableOutput(ns("graphOutput")),
            uiOutput(ns("graphOutput_code"))
          ),

          # For help editing the graph (TODO)
          tabPanel(
            "Help",
          )
        )
      ),

      # For more gfpop/changepoint output details
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

    # A button for debugging
    actionButton(ns("browser"), "browser"),
    tags$script(paste0("$('#", ns("browser"), "').hide();")),
  )
}

#' analysis Server Function
#' @param id The id of this module
#' @param gfpop_data The main data for the gfpop analysis, passed from home mod
#' @importFrom shiny reactiveValues observeEvent req reactive isTruthy validate
#' isolate updateNumericInput renderUI renderText
#' @importFrom plotly ggplotly renderPlotly plot_ly add_markers
#' @importFrom visNetwork renderVisNetwork visNetworkProxy visUpdateNodes visUpdateEdges
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom data.table data.table rbindlist
#' @importFrom dplyr mutate filter
#' @importFrom gfpop gfpop
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs onevent
#' @import gfpop
#' @export
mod_analysis_server <- function(id, gfpop_data = reactiveValues()) {
  # CMD Check compatibility section
  moduleServer <- NULL
  updateTextInput <- NULL
  observe <- NULL
  type <- NULL
  to <- NULL
  from <- NULL
  state <- NULL
  label <- NULL
  # End CMD compatibility section

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Initialize All Reactive Values -----------------------------------------

      # Keep track of all saved data
      saved_analyses <- reactiveValues(
        saved_full = list(),
        saved_descriptions = data.table()
      )

      # Keep track of the current starting and ending nodes
      startEnd <- reactiveValues(
        start = "N/A",
        end = "N/A"
      )

      # Keep track of what nodes, edges, and changepoint segments should be
      # selected
      selected <- reactiveValues(
        nodes = c(),
        edges = c(),
        segments = c()
      )

      # Keep track of the mapping between a node ID and the node's label
      node_id_to_label <- reactiveValues(
        main = list()
      )

      # When i is edited, the the constraint graph undergoes a hard refresh
      dummy_graph_refresh <- reactiveValues(i = 0)

      # When i is edited, the plotly data graph undergoes a hard refresh
      dummy_plotly_refresh <- reactiveValues(i = 0)

      # Save and Load ----------------------------------------------------------

      # Render the saved analyses
      output$uiLoadId <- renderUI({
        selectInput(ns("loadId"), "Select a previous analysis",
          choices = saved_analyses$saved_descriptions$id
        )
      })

      # Observe a save
      observeEvent(input$saveButton, {
        saveId <- req(input$saveId)

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
        gfpop_data_tmp <- do.call("reactiveValues", 
                               saved_analyses$saved_full[[input$loadId]])
        
        for(name in names(gfpop_data_tmp)) {
          gfpop_data[[name]] <- gfpop_data_tmp[[name]]
        }

        # Hard update everything after a load
        dummy_graph_refresh$i <- dummy_graph_refresh$i + 1
        dummy_plotly_refresh$i <- dummy_plotly_refresh$i + 1
      })

      # Render Graph and Respond to Settings -----------------------------------

      # Render the graph
      output$gfpopGraph <- renderVisNetwork({
        # Hard refresh when the user asks, or when dummy value is updated
        input$refreshGraph
        dummy_graph_refresh$i

        generate_visNetwork(isolate(gfpop_data$graphdata_visNetwork))
      })

      # Soft-refresh the graph when nodes or edges are updated
      observe({
        # Update edge labels, if necessary
        if (isTruthy(gfpop_data$graphdata_visNetwork$edges)) {
          gfpop_data$graphdata_visNetwork$edges$label <- create_label(
            gfpop_data$graphdata_visNetwork$edges,
            columns = input$labels
          )
        }

        # Update graph edges and nodes
        visNetworkProxy(ns("gfpopGraph")) %>%
          visUpdateNodes(nodes = gfpop_data$graphdata_visNetwork$nodes) %>%
          visUpdateEdges(edges = gfpop_data$graphdata_visNetwork$edges)

        # Update the mapping between node ids and labels
        node_ids <- gfpop_data$graphdata_visNetwork$nodes$id
        node_labels <- gfpop_data$graphdata_visNetwork$nodes$label
        names(node_labels) <- node_ids
        node_id_to_label$main <- node_labels
      })

      # Generate a default graph, when the appropriate button is pressed
      observeEvent(eventExpr = input$updateGraph, {
        gfpop_data$graphdata <- gfpop::graph(
          penalty = as.double(input$pen),
          type = input$graphType
        )

        gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
          gfpop_data$graphdata,
          showNull = input$showNull,
          label_columns = input$labels
        )

        # Hard refresh the graph
        dummy_graph_refresh$i <- dummy_graph_refresh$i + 1
      })

      # Adjust whether null nodes are visible, after user clicks radio box.
      observeEvent(eventExpr = input$showNull, {
        gfpop_data$graphdata_visNetwork$edges$hidden <- sapply(
          gfpop_data$graphdata_visNetwork$edges$type,
          function(x) if (input$showNull) FALSE else (x == "null")
        )
      })

      # Update Graph upon User Edit --------------------------------------------

      # Respond to a change in the visNetwork plot (via manipulation)
      observeEvent(input$gfpopGraph_graphChange, {
        event <- input$gfpopGraph_graphChange
        gfpop_data$graphdata_visNetwork <- modify_visNetwork(
          event,
          gfpop_data$graphdata_visNetwork,
          addNull = input$addNull
        )

        # Ensure that graphdata stays in sync with visNetwork data
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      # Add a new starting node
      observeEvent(input$setStartEnd_button, {

        # Sets either the starting or ending node
        # new_val: (str) the label of the new start or new end
        # val_type: (str) "start" or "end"
        set_startEnd <- function(new_val, val_type) {
          if (new_val != "N/A") {
            gfpop_data$graphdata <<- gfpop::graph(
              rbindlist(list(
                gfpop_data$graphdata,
                list(state1 = new_val, type = val_type)
              ), use.names=T, fill =T)
            )
          } else {
            gfpop_data$graphdata <<- gfpop::graph(
              data.table(gfpop_data$graphdata) %>%
                filter(type != val_type)
            )
          }
        }

        set_startEnd(input$setStart, "start")
        set_startEnd(input$setEnd, "end")

        # Remember the start and end node. Important for UI fluidity.
        startEnd$start <- input$setStart
        startEnd$end <- input$setEnd

        # Update the visNetwork data to match the gfpop data
        gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata,
          showNull = input$showNull,
          label_columns = input$labels,
          edge_ids = gfpop_data$graphdata_visNetwork$edges$id
        )
      })

      # Add a new node
      observeEvent(input$addNode_button, {
        # New node must have a unique ID. TODO: Does this need to be edited?
        if (input$addNode_id %notin% gfpop_data$graphdata_visNetwork$nodes$id) {
          # Add the node
          gfpop_data$graphdata_visNetwork$nodes <- add_node(
            gfpop_data$graphdata_visNetwork$nodes,
            id = input$addNode_id,
            label = input$addNode_id
          )

          # If addNull is true, add a recursive null edge
          if (input$addNull) {
            gfpop_data$graphdata_visNetwork$edges <- add_null_edge(
              edgedf = gfpop_data$graphdata_visNetwork$edges,
              nodeid = input$addNode_id
            )
          }
        } else {
          shinyalert(
            title = "Duplicate ID",
            text = "Node IDs must be unique. Please provide
                                 a unique node ID", type = "error"
          )
        }
      })

      # Remove an existing node
      observeEvent(input$removeNode_button, {
        nodes <- gfpop_data$graphdata_visNetwork$nodes
        edges <- gfpop_data$graphdata_visNetwork$edges

        gfpop_data$graphdata_visNetwork$nodes <- nodes %>%
          filter(id != input$setRemoveNode)

        # Also filter out any edges containing the deleted node
        gfpop_data$graphdata_visNetwork$edges <- edges %>%
          filter(to != input$setRemoveNode & from != input$setRemoveNode)

        # Make sure that the graphdata stays up-to-date
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      # Add a new edge
      observeEvent(input$addEdge_button, {
        edges <- gfpop_data$graphdata_visNetwork$edges

        # Add new edge with the given inputs
        gfpop_data$graphdata_visNetwork$edges <- edges %>% add_edge(
          label = create_label(edges,input$labels),
          id = input$addEdge_id, to = input$addEdge_to, from = input$addEdge_from,
          type = input$addEdge_type, parameter = input$addEdge_parameter,
          penalty = input$addEdge_penalty
        )

        # Update edge labels appropriately
        gfpop_data$graphdata_visNetwork$edges$label <- gfpop_data$graphdata_visNetwork$edges %>%
          create_label()

        # Make sure that the graphdata stays up-to-date
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })


      # Update graph when a cell is edited in the direct gfpop input datatable
      proxy <- dataTableProxy("graphOutput")
      observeEvent(input$graphOutput_cell_edit, {
        info <- input$graphOutput_cell_edit
        i <- info$row
        j <- info$col
        v <- info$value

        # Account for the "state1_id" and "state2_id" columns:
        if(ncol(gfpop_data$graphdata) > 9) {
          j <- if (j > 3) j + 2 else if (j > 1) j + 1 else j
        }
        
        # Update graphdata via proxy
        gfpop_data$graphdata <- data.table(gfpop_data$graphdata)
        gfpop_data$graphdata[i, j] <<- DT::coerceValue(v, gfpop_data$graphdata[i, j, with=F])
        replaceData(proxy, gfpop_data$graphdata, resetPaging = FALSE)

        # Make sure visNetwork data stays up-to-date
        gfpop_data$graphdata_visNetwork$edges <- update_visNetwork_edges(
          gfpop_data$graphdata_visNetwork$edges, gfpop_data$graphdata,
          showNull = input$showNull,
          label_columns = input$labels
        )
      })

      # Update graph when a cell is edited in the visEdges datatable
      proxy_visEdges <- dataTableProxy("graphOutput_visEdges")
      observeEvent(input$graphOutput_visEdges_cell_edit, {
        info <- input$graphOutput_visEdges_cell_edit
        i <- info$row
        j <- info$col
        v <- info$value

        # Update visNetwork data via proxy
        gfpop_data$graphdata_visNetwork$edges[i, j] <<- DT::coerceValue(
          v, gfpop_data$graphdata_visNetwork$edges[i, j, with=F]
        )
        replaceData(proxy_visEdges, gfpop_data$graphdata_visNetwork$edges, resetPaging = FALSE)

        # Make sure main graphdata stays up-to-date
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      # Update graph when a cell is edited in the visNodes datatable
      proxy_visNodes <- dataTableProxy("graphOutput_visNodes")
      observeEvent(input$graphOutput_visNodes_cell_edit, {
        info <- input$graphOutput_visNodes_cell_edit
        i <- info$row
        j <- info$col
        v <- info$value

        # Update visNetwork data via proxy
        gfpop_data$graphdata_visNetwork$nodes[i, j] <<- DT::coerceValue(
          v, gfpop_data$graphdata_visNetwork$nodes[i, j, with=F]
        )
        replaceData(proxy_visNodes, gfpop_data$graphdata_visNetwork$nodes, resetPaging = FALSE)

        # Make sure the main graphdata stays up-to-date
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
      })

      # Render Graph Edit UI Components ----------------------------------------
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

      # Render Graph DataTables ------------------------------------------------
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

      # Communication from visNetwork to Plotly --------------------------------

      # When the user highlights a node, update the plotly plot
      observeEvent(input$gfpopGraph_highlight_color_id, {
        if (input$crosstalk) {
          # Only add traces if a node is highlighted
          if (!is.null(input$gfpopGraph_highlight_color_id)
          & isTruthy(gfpop_data$changepoint_annotations_list)) {

            # Be lazy: only highlight if not already highlighted
            if (input$gfpopGraph_highlight_color_id %notin% selected$segments) {

              # First, delete existing highlight
              if (length(selected$segments) > 0) {
                plotlyProxy("gfpopPlot", session) %>%
                  plotlyProxyInvoke(
                    "deleteTraces", 3
                  )
                selected$segments <- c()
              }

              # Figure out what region to highlight
              highlighted_id <- input$gfpopGraph_highlight_color_id

              # Highlight based on node label
              highlighted_label <- node_id_to_label$main[[highlighted_id]]

              segments_to_highlight <-
                gfpop_data$changepoint_annotations_list[["changepoint_annotations_regions"]] %>%
                filter(state == highlighted_label)

              # Highlight the appropriate region by adding a new red trace
              plotlyProxy("gfpopPlot", session) %>%
                plotlyProxyInvoke(
                  "addTraces",
                  list(
                    x = segments_to_highlight$x,
                    y = segments_to_highlight$y,
                    text = segments_to_highlight$text,
                    line = list(color = "red", width = 10)
                  )
                )

              # Remember what region we've highlighted
              selected$segments <- c(input$gfpopGraph_highlight_color_id)
            }
          } else {
            # If a segment is highlighted, but a node isn't, remove segment highlight
            if (length(selected$segments) > 0) {
              plotlyProxy("gfpopPlot", session) %>%
                plotlyProxyInvoke(
                  "deleteTraces", 3
                )

              selected$segments <- c()
            }
          }
        }
      })

      # Deselect changeregions when the mouse leaves the visNetwork plot
      onevent("mouseleave", "gfpopGraph", {
        if (length(selected$segments) > 0) {

          # Note: traces added to highlight a segment are in the 4th layer (3)
          plotlyProxy("gfpopPlot", session) %>%
            plotlyProxyInvoke(
              "deleteTraces", 3
            )
          selected$segments <- c()
        }
      })

      # Render Plotly and Respond to Settings ----------------------------------

      # Initialize a base Plotly plot with just the base data
      initialize_plot <- observeEvent(gfpop_data$main_data, {
        req(gfpop_data$main_data)

        gfpop_data$base_plot <-
          plot_ly(gfpop_data$main_data, x = ~X, y = ~Y, hoverinfo = "none", source = "gfpopPlot") %>%
          add_markers()

        gfpop_data$changepoint_plot <- gfpop_data$base_plot
      })

      # Render the Plotly plot
      output$gfpopPlot <- renderPlotly({
        dummy_plotly_refresh$i

        if (isTruthy(gfpop_data$changepoints)) {
          gfpop_data$changepoint_plot
        } else {
          gfpop_data$base_plot
        }
      })

      # Add changepoints to a saved Plotly plot when the user asks
      observeEvent(input$runGfpop, {
        initialize_changepoints()
        changepoint_annotations_list <- add_changepoints(
          gfpop_data$base_plot,
          isolate(gfpop_data$main_data),
          isolate(gfpop_data$changepoints)
        )

        gfpop_data$changepoint_plot <- changepoint_annotations_list[["plot"]]
        gfpop_data$changepoint_annotations_list <- changepoint_annotations_list
      })

      # A helper function to initalize changepoints (gfpop_data$changepoints)
      initialize_changepoints <- reactive({
        req(gfpop_data$main_data)

        # Update this function when changepoints update
        gfpop_data$changepoints
        tryCatch(
          expr = {
            # TODO: Allow user to add weights (what do those do?)
            gfpop_data$changepoints <<- gfpop::gfpop(as.numeric(gfpop_data$main_data$Y),
              gfpop_data$graphdata %>% select_graph_columns() %>% gfpop::graph(),
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

      # Clear changepoints
      observeEvent(input$clsCp, {
        gfpop_data$changepoints <- NULL
        gfpop_data$changepoint_plot <- gfpop_data$base_plot
      })

      # Changepoint DataTables -------------------------------------------------

      output$gfpopOutput <- DT::renderDT(
        {
          input$loadButton
          changepoints <- req(gfpop_data$changepoints)
          data.table(
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

      # Communication from Plotly to visNetwork---------------------------------

      # (Helper) Collect data when the user hovers over the Plotly visualization
      hover_data <- reactive({
        req(gfpop_data$base_plot)
        event_data("plotly_hover", "gfpopPlot")
      })

      # When the user hovers on Plotly, highlight nodes in visNetwork
      observeEvent(hover_data(), {
        if (input$crosstalk) {
          event <- hover_data()
          nevents <- dim(event)[1]

          # Ignore data from the first row of event (that's for the scatter layer)
          if (nevents > 1) {

            # The second row contains the key of the selected changeregion
            selection <- event$key[2]

            # Only make a change a _different_ changeregion is selected than before
            if (event$key[2] %notin% selected$nodes) {

              # First, un-select existing selections
              if (length(selected$nodes) > 0) {
                visNetworkProxy(ns("gfpopGraph")) %>%
                  visUpdateNodes(
                    gfpop_data$graphdata_visNetwork$nodes %>%
                      mutate(
                        color.border = "lightblue", color.border = "lightblue",
                        shadow = FALSE
                      )
                  )

                selected$nodes <- c()
              }

              # Now, update the visNetwork graph with a new color.
              visNetworkProxy(ns("gfpopGraph")) %>%
                visUpdateNodes(
                  gfpop_data$graphdata_visNetwork$nodes %>%
                    filter(label == selection) %>%
                    mutate(
                      color.background = "#D2E5FF", color.border = "red",
                      shadow = TRUE
                    )
                )

              # Remember the selected node
              selected$nodes <- c(selection)
            }
          }
        }
      })

      # Unselect nodes when the mouse leaves the Plotly plot
      onevent("mouseleave", "gfpopPlot", {
        if (length(selected$nodes) > 0) {
          visNetworkProxy(ns("gfpopGraph")) %>%
            visUpdateNodes(
              gfpop_data$graphdata_visNetwork$nodes
            )
        }
        selected$nodes <- c()
      })

      # Debugging --------------------------------------------------------------
      observeEvent(input$browser, {
        browser()
      })
    }
  )

  # Return the gfpop data for the sharing tab
  gfpop_data
}
