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

        hr(),
        h4("gfpop settings"),
        selectInput(
          inputId = ns("gfpopType"),
          label = "Cost model",
          choices = c("mean", "variance", "poisson", "exp", "negbin")
        ),

        actionButton(inputId = ns("runGfpop"), label = "Run gfpop!")
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
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom visNetwork renderVisNetwork
#' @importFrom DT renderDataTable renderDT dataTableProxy replaceData
#' @importFrom dplyr mutate
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
    generate_visNetwork(gfpop_data$graphdata_visNetwork)
  })

  # Monitor edge edits. Edit the gfpop_data$graphdata_visNetwork variable
  # from the observations, and then update the gfpop_data$graphdata
  observeEvent(input$gfpopGraph_graphChange, {
    event <- input$gfpopGraph_graphChange

    if (event$cmd == "editEdge") {
      changed_id <- event$id

      gfpop_data$graphdata_visNetwork$edges <- gfpop_data$graphdata_visNetwork$edges %>%
        mutate_cond(id == changed_id,
          label = paste0(event$type, " | ", event$penalty),
          to = event$to, from = event$from,
          type = event$type, parameter = event$parameter,
          penalty = event$penalty, K = event$K, a = event$a,
          min = event$min, max = event$max,
          hidden = as.logical(event$hidden)
        )
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
  update_changepointdf <- reactive({
    req(gfpop_data$main_data)
    graphdata <- gfpop_data$graphdata
    gfpop_data$changepointdf <- generate_changepoint(
      gfpop_data$main_data$Y,
      graphdata, input$gfpopType
    )
  })

  # Generate the visualization of the data with overlain changepoints
  # TODO: Can we always show the data, and then just overlay changepoints
  # whenever "Run gfpop!" is pressed?
  # Should work by storing a base plotly object in memory, then piping from that.
  output$gfpopPlot <- renderPlotly({
    input$runGfpop

    isolate(update_changepointdf())

    if (!isTruthy(isolate(gfpop_data$main_data))) {
      validate(FALSE, "Either you haven't uploaded any data, or you haven't 
               pressed the 'run gfpop!' button yet.")
      return(1)
    }

    ggplotly(plot_changepoint(
      data_input = isolate(gfpop_data$main_data),
      changepoint_data = isolate(gfpop_data$changepointdf)
    ),
    tooltip = c("X", "Y", "text")
    )
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
