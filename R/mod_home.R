#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        h1("Welcome to gfpop", align = "center"),
        HTML("<h4 style='text-align: center'> An analysis tool for graph-constrained <br> changepoint detection </h4>"),
        br(), br(),
        h3("Getting started: overview and instructions"),
        HTML(" 
              <p> Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
              <br><br><br>
              Duis aute irure dolor:
                <li> reprehenderit in voluptate velit esse cillum </li>
                <li> dolore eu fugiat nulla pariatur </li>
                <li> excepteur sint occaecat cupidatat non provident </li>
                <li> sunt in culpa qui officia deserunt mollit anim id est laborum </li>
              <br>
              In pellentesque massa placerat duis ultricies lacus. Lorem sed risus ultricies tristique nulla aliquet. Nibh sit amet commodo nulla facilisi nullam vehicula ipsum a. 
              <br><br>
              Egestas integer eget aliquet nibh praesent. Arcu dui vivamus arcu felis bibendum ut.
              <br><br>
              Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua
              "
        )),
      column(
        4,
        h3("Upload user data", align = "center"),
        div(
          class = "well",
          HTML("<h4>Basic uploads:</h4><br>"),
          fileInput(ns("primary_input"), "Choose a file with primary input data (.csv)"),
          HTML("<hr>"),
          HTML("<h4>Optional/advanced uploads:</h4><br>"),
          fileInput(ns("constraint_graph"), "Choose a file with a constraint graph (.csv)"),
          fileInput(ns("completed_analysis"), "Choose a file with a completed analysis (.Rdata)"),
        )
      )
    )
  )
}
    
#' home Server Function
#'
#' @noRd 
mod_home_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# callModule(mod_home_server, "home_ui_1")
 
