#' sharing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h2 downloadButton actionButton
#' @importFrom DT renderDataTable
#' @importFrom shinyalert shinyalert
mod_sharing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 8,
        h1("Exports"),
        hr(),
        h2("Download current analysis (.Rdata)"),
        "Full .Rdata files can be uploaded directly on the 'home' page, allowing
    other users (or your future self) to exactly replicate the analysis that
    you currently have loaded in the gfpop GUI", br(),
        downloadButton(ns("downloadData"), "Click to Download"),
        hr(),
        h2("Download current graph (.csv)"),
        downloadButton(ns("downloadGraphCSV"), "Graph (CSV)"),
        "|",
        downloadButton(ns("downloadGraphR"), "Code to Construct Graph (R)"),
        hr(),
        h2("Download current changepoints (.Rdata)"),
        downloadButton(ns("downloadChangepoints")),
        hr()
      ),
      sidebarPanel(
        width = 4,
        h1("Current Session Overview"),
        h3("Constraint graph"),
        dataTableOutput(ns("graphOutput")),
        h3("Changepoints"),
        htmlOutput(ns("gfpopOutput_verbose")),
        h3("Data with Changepoints"),
        plotlyOutput(ns("gfpopPlot")) %>% withSpinner(type = 6)
      )
    )
  )
}

#' sharing Server Function
#' @importFrom shiny downloadHandler reactiveValuesToList observeEvent
#' reactiveValuesToList
#' @importFrom DT dataTableOutput
#' @noRd
mod_sharing_server <- function(id, gfpop_data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadData <- downloadHandler(
        filename = function() "gfpopgui_data.Rdata",
        content = function(file) {
          gfpop_data_list <- reactiveValuesToList(gfpop_data, all.names = T)
          save(gfpop_data_list, file = file)
        }
      )

      output$downloadGraphCSV <- downloadHandler(
        filename = function() "gfpop_graphdata.csv",
        content = function(file) {
          gfpop_data_list <- reactiveValuesToList(gfpop_data, all.names = T)
          write.csv(x = gfpop_data_list$graphdata, file = file, row.names = FALSE)
        }
      )

      output$downloadGraphR <- downloadHandler(
        filename = function() "gfpop_graphdata.R",
        content = function(file) {
          writeLines(graph_to_R_code(gfpop_data$graphdata), file)
        }
      )
      output$downloadChangepoints <- downloadHandler(
        filename = function() "gfpop_changepoints.Rdata",
        content = function(file) {
          if (!isTruthy(gfpop_data$changepoints)) {
            shinyalert::shinyalert(
              text = "No changepoint data defined.",
              type = "error"
            )
          }
          gfpop_data_list <- reactiveValuesToList(gfpop_data, all.names = T)
          save(gfpop_data_list$changepoints, file = file)
        }
      )

      # Render the Plotly preview plot
      output$gfpopPlot <- renderPlotly({
        if (isTruthy(gfpop_data$changepoints)) {
          gfpop_data$changepoint_plot %>%
            layout(height = 200) %>%
            config(displayModeBar = FALSE)
        } else if (isTruthy(gfpop_data$main_data)) {
          gfpop_data$base_plot %>%
            layout(height = 200) %>%
            config(displayModeBar = FALSE)
        }
      })

      # Render the graphdf preview datatable
      output$graphOutput <- DT::renderDT(
        {
          gfpop_data$graphdata %>% select_graph_columns()
        },
        editable = FALSE,
        options = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
      )
      # Render the verbose version of the gfpop output preview
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
    }
  )
}
## To be copied in the UI
# mod_sharing_ui("sharing_ui_1")

## To be copied in the server
# callModule(mod_sharing_server, "sharing_ui_1")
