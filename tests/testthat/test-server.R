library(testthat)
library(gfpop)

# A helper function for server tests
assert_that <- function(error, expr = "", msgs = list()) {
  if (length(expr) == 0) {
    print(paste0("Failed in test-server.R: ", error))
    stop(paste0("Failed: ", error, "| Length 0"), call. = FALSE)
  }
  if (!expr) {
    print(paste0("Failed in test-server.R: ", error))
    print(paste0("Begin debug: ", error, " ------------"))
    for (msg in msgs) {
      print(msg)
    }
    print(paste0("End debug: ", error, " ------------"))
    stop(paste0("Failed: ", error), call. = FALSE)
  }
}

# app_server.R -----------------------------------------------------------------
app_server_test <- function(id) {
  moduleServer(id, app_server)
}

test_that(
  "Server tests: app_server",
  expect_null(
    shiny::testServer(app_server_test, {
      assert_that(
        "graphdata is successfully initialized at the app_server level",
        all(names(gfpop_data) == c("graphdata_visNetwork", "graphdata")),
        paste0("Failed, names are: ", names(gfpop_data))
      )
    })
  )
)

# mod_home_server.R ------------------------------------------------------------
test_that(
  "Server tests: mod_home_server",
  expect_null(
    shiny::testServer(mod_home_server, {
      assert_that(
        "Before any inputs are given, the app should have graphdata loaded",
        all(names(gfpop_data) == c("graphdata_visNetwork", "graphdata")),
        paste0("Failed, names are: ", names(gfpop_data))
      )

      assert_that(
        "Before any inputs are given, the app should not have main-data loaded",
        is.null(gfpop_data$main_data)
      )

      assert_that(
        "Before any inputs are given, the main datatable on the home page should have null data",
        substr(output$main_datatable, 6, 9) == "null"
      )

      # Set some sample data from an external source
      session$setInputs(primary_input = list(
        datapath =
          "https://raw.githubusercontent.com/julianstanley/gfpop-gui/master/data/datatest.csv"
      ))

      assert_that(
        "Can set main data",
        nrow(gfpop_data$main_data) == 3000,
        paste0("Nrow of main data is: ", nrow(gfpop_data$main_data))
      )

      assert_that(
        "After setting main data, the main datatable is no longer null",
        substr(output$main_datatable, 6, 9) != "null",
        paste0("Output looks null. See: ", output$main_datatable)
      )

      # Reset the main data
      gfpop_data$main_data <- NULL
      assert_that(
        "Main data reset successfully",
        is.null(gfpop_data$main_data)
      )

      session$setInputs(ndata = 100, sigma = 1)
      generate_data()

      assert_that(
        "The generate data reactive works",
        nrow(gfpop_data$main_data) == 100,
        list(paste0("Main data doesn't look right, here it is: ", gfpop_data$main_data))
      )

      gfpop_data$graphdata <- NULL
      gfpop_data$graphdata_visNetwork <- NULL
      assert_that(
        "Graph data reset successfully",
        (is.null(gfpop_data$graphdata) && is.null(gfpop_data$graphdata_visNetwork))
      )

      session$setInputs(constraint_graph = list(
        datapath =
          "https://raw.githubusercontent.com/julianstanley/gfpop-gui/master/data/graphtest.csv"
      ))

      assert_that(
        "graphdata is reasonable after setting constraint_graph [home]",
        ncol(gfpop_data$graphdata) == 10,
        gfpop_data$graphdata
      )

      assert_that(
        "graphdata_visNetwork is reasonable after setting constraint_graph [home]",
        {
          testdata <- gfpop_data$graphdata_visNetwork
          test1 <- names(testdata) == c("nodes", "edges")
          test2 <- ncol(testdata$nodes) == 3
          test3 <- ncol(testdata$edges) == 14
          test1 && test2 && test3
        },
        paste0(
          "failed, expected graphdata_visNetwork to have nodes and edges ",
          "with 3 and 14 columns, respectively. Here's the failed data: ",
          dplyr::tibble(gfpop_data$graphdata_visNetwork)
        )
      )
    })
  )
)


# mod_analysis_server.R---------------------------------------------------------
test_that(
  "Server tests: mod_analysis_server",
  expect_null(
    shiny::testServer(mod_analysis_server, {
      # Reset the graph first to test updateGraph()
      gfpop_data$graphdata <- NULL
      gfpop_data$graphdata_visNetwork <- NULL
      assert_that(
        "Graph data reset successfully",
        (is.null(gfpop_data$graphdata) && is.null(gfpop_data$graphdata_visNetwork))
      )

      session$setInputs(pen = 15, graphType = "std", showNull = TRUE)
      updateGraph()

      assert_that(
        "graphdata is reasonable after  update_graph [analysis]",
        ncol(gfpop_data$graphdata) == 9,
        gfpop_data$graphdata
      )

      assert_that(
        "graphdata_visNetwork is reasonable after update_graph [analysis]",
        {
          testdata <- gfpop_data$graphdata_visNetwork
          test1 <- names(testdata) == c("nodes", "edges")
          test2 <- ncol(testdata$nodes) == 3
          test3 <- ncol(testdata$edges) == 14
          test1 && test2 && test3
        },
        list(
          gfpop_data$graphdata_visNetwork,
          names(testdata), ncol(testdata$nodes),
          ncol(testdata$edges)
        )
      )

      session$setInputs(showNull = FALSE)
      hideNull()
      assert_that(
        "hideNull works",
        all(gfpop_data$graphdata_visNetwork$edges$hidden == c("TRUE", "FALSE")),
        list(gfpop_data$graphdata_visNetwork$edges)
      )

      assert_that(
        "Check edge before editing",
        {
          pen_vis <- gfpop_data$graphdata_visNetwork$edges$penalty
          pen_orig <- gfpop_data$graphdata$penalty
          all(pen_orig == c(0, 15)) && all(pen_orig == pen_vis)
        },
        list(gfpop_data$graphdata_visNetwork$edges)
      )

      # Graph edit observers ---------------------------------------------------
      
      ## Edit Edge
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "editEdge",
        to = "Std", from = "Std",
        id = "Std_Std_std", type = "std", penalty = "200", parameter = "0",
        K = "Inf", a = "0", min = "None", max = "None", hidden = "FALSE"
      ))

      assert_that(
        "can edit an edge, scenario 1",
        {
          pen_vis <- gfpop_data$graphdata_visNetwork$edges$penalty
          pen_orig <- gfpop_data$graphdata$penalty
          all(pen_orig == c(0, 200)) && all(pen_orig == pen_vis)
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )

      ## Add Node
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "addNode",
        id = "new_node", label = "new node"
      ))

      assert_that(
        "can add a node",
        {
          nodes_test <- gfpop_data$graphdata_visNetwork$nodes
          (all(nodes_test$id == c("Std", "new_node")) &&
            all(nodes_test$label == c("Std", "new node")) &&
            all(nodes_test$size == c(40, 40)))
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )

      ## Edit Node
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "editNode",
        id = "new_node", label = "my edited new node"
      ))

      assert_that(
        "can edit a node",
        {
          nodes_test <- gfpop_data$graphdata_visNetwork$nodes
          (all(nodes_test$id == c("Std", "new_node")) &&
            all(nodes_test$label == c("Std", "my edited new node")) &&
            all(nodes_test$size == c(40, 40)))
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )

      ## Add Edge
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "addEdge", id = "new_std_null",
        to = "new_node", from = "Std"
      ))

      assert_that(
        "can add an edge",
        {
          pen_vis <- gfpop_data$graphdata_visNetwork$edges$penalty
          pen_orig <- gfpop_data$graphdata$penalty
          all(pen_orig == c(0, 200, 0)) && all(pen_orig == pen_vis)
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )
      
      ## Edit Edge #2
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "editEdge",
        to = "new_node", from = "Std",
        id = "new_std_null", type = "up", penalty = "10", parameter = "0",
        K = "Inf", a = "0", min = "None", max = "None", hidden = "FALSE"
      ))
      
      assert_that(
        "can edit an edge, scenario 2",
        {
          pen_vis <- gfpop_data$graphdata_visNetwork$edges$penalty
          pen_orig <- gfpop_data$graphdata$penalty
          all(pen_orig == c(0, 200, 10)) && all(pen_orig == pen_vis)
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )

      ## Delete a node, but not an edge
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "deleteElements", nodes = c("new_node")
      ))

      assert_that(
        "can delete a node",
        {
          nodes_test <- gfpop_data$graphdata_visNetwork$nodes
          (all(nodes_test$id == c("Std")) &&
            all(nodes_test$label == c("Std")) &&
            all(nodes_test$size == c(40)))
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )

      ## Delete an edge, but not a node
      session$setInputs(gfpopGraph_graphChange = list(
        cmd = "deleteElements", nodes = c(),
        edges = c("new_std_null")
      ))

      assert_that(
        "can delete an edge",
        {
          pen_vis <- gfpop_data$graphdata_visNetwork$edges$penalty
          pen_orig <- gfpop_data$graphdata$penalty
          all(pen_orig == c(0, 200)) && all(pen_orig == pen_vis)
        },
        list(
          gfpop_data$graphdata_visNetwork,
          gfpop_data$graphdata
        )
      )
      
      # Changepoint tests ------------------------------------------------------
      gfpop_data$main_data = data.frame(
          X = 1:100,
          Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1), 
                            c(1, 2, 1, 3, 1), sigma = 1)
        )
      
      session$setInputs(gfpopType = "mean")
      assert_that(
        "changepoints do not exist before init",
        all(names(gfpop_data) == c("graphdata_visNetwork", "graphdata", "main_data", "base_plot")),
        list(gfpop_data)
      )
    
      initialize_changepoints()

      assert_that("initalize_changepoints works",
                  length(gfpop_data$changepoints) == 5,
                  "")
      
      # GraphOutput cell edit tests --------------------------------------------
      assert_that("ensure penalty starts at 200",
                  {
                  all(gfpop_data$graphdata$penalty == c(0, 200)) &&
                      all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 200))
                    },
                  list(gfpop_data$graphdata,
                       gfpop_data$graphdata_visNetwork$edges))
  
      session$setInputs(graphOutput_cell_edit = list(row = 2, col = 5, value = 20))
      
      assert_that("Can edit penalty from graphOutput",
                  {
                    all(gfpop_data$graphdata$penalty == c(0, 20)) &&
                      all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 20))
                  },
                  list(gfpop_data$graphdata,
                       gfpop_data$graphdata_visNetwork$edges))
      
      session$setInputs(graphOutput_visEdges_cell_edit = list(row = 2, col = 7, value = 100))
      
      assert_that("Can edit penalty from graphOutput_visEdges",
                  {
                    all(gfpop_data$graphdata$penalty == c(0, 100)) &&
                      all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 100))
                  },
                  list(gfpop_data$graphdata,
                       gfpop_data$graphdata_visNetwork$edges))
      
      session$setInputs(graphOutput_visNodes_cell_edit = list(row = 1, col = 3, value = 100))
      assert_that("Can edit nodes from graphoutput_visNodes",
                  all(gfpop_data$graphdata_visNetwork$nodes$size == c(100)),
                  list(gfpop_data$graphdata_visNetwork$nodes))
    })
  )
)
