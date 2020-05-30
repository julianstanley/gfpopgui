tabPanel(
  title = "Home",
  id = "home",
  tagList(
    fluidRow(
      column(
        8,
        h1("Welcome to gfpop", align = "center"),
        HTML("<h4 style='text-align: center'> An analysis tool for graph-constrained <br> changepoint detection </h4>"),
        br(), br(),
        h3("Getting started: overview and instructions"),
        includeHTML(system.file("app/www/lorem.html", package = "gfpopgui")),
      ),
      column(
        4,
        h3("Upload user data", align = "center"),
        div(
          class = "well",
          HTML("<h4>Basic uploads:</h4><br>"),
          fileInput("primary_input", "Choose a file with primary input data (.csv)"),
          p("Or generate data (temporary):"),
          fluidRow(
            column(6, numericInput(inputId = "ndata", label = "# Datapoints", value = 1000)),
            column(6, numericInput(inputId = "sigma", label = "sigma", value = 1, min = 0.1)),
          ),
          actionButton(inputId = "genData", label = "Generate!"),
          HTML("<hr>"),
          HTML("<h4>Optional/advanced uploads:</h4><br>"),
          fileInput("constraint_graph", "Choose a file with a constraint graph (.csv)"),
          fileInput("completed_analysis", "[Pending] Choose a file with a completed analysis (.Rdata)"),
        ),
        h3("Uploaded data:", align = "center"),
        div(
          class = "well",
          h4("Input data:"),
           dataTableOutput("main_datatable"),
          h4("Constraint graph:"),
          dataTableOutput("graph")
        )
      )
    )
  )
)