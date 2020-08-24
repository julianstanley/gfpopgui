library(testthat)
library(gfpop)
library(shiny)
library(data.table)

# A helper function for server tests
assert_that <- function(error, expr = "", msgs = list()) {
  if (length(expr) == 0) {
    print(paste0("Failed in test-server.R: ", error))
    stop(paste0("Failed: ", error, "| Length 0"), call. = FALSE)
  }
  msgs <- toString(msgs)
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
  {
    expect_null(
      shiny::testServer(app_server_test, {
        assert_that(
          "graphdata is successfully initialized at the app_server level",
          all(sapply(
            names(gfpop_data),
            function(x) x %in% c("graphdata_visNetwork", "graphdata")
          )),
          paste0("Failed, names are: ", names(gfpop_data))
        )
      })
    )
  }
)

# mod_home_server.R, general ---------------------------------------------------
test_that(
  "Server tests: mod_home_server",
  {
    expect_null(
      shiny::testServer(mod_home_server, {
        assert_that(
          "Before any inputs are given, the app should have graphdata loaded",
          all(sapply(
            names(gfpop_data),
            function(x) x %in% c("graphdata_visNetwork", "graphdata")
          )),
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
            "https://raw.githubusercontent.com/julianstanley/gfpopgui/master/data/datatest.csv"
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

        session$setInputs(
          ndata = 100, sigma = 1, genData = input$genData + 1,
          nChangepoints = 5, eChangepoints = 1,
          meansChangepoints = "", typeChangepoints = "mean",
          gammaChangepoints = 1
        )
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
            "https://raw.githubusercontent.com/julianstanley/gfpopgui/master/data/graphtest.csv"
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
            test1 <- all(names(testdata) == c("nodes", "edges"))
            test2 <- ncol(testdata$nodes) == 9
            test3 <- ncol(testdata$edges) == 15
            test1 && test2 && test3
          },
          paste0(
            "failed, expected graphdata_visNetwork to have nodes and edges ",
            "with 7 and 15 columns, respectively. Here's the failed data: ",
            dplyr::tibble(gfpop_data$graphdata_visNetwork)
          )
        )
      })
    )
  }
)

# Home uploading and generating data -------------------------------------------

# Uploading
shiny::testServer(mod_home_server, {
  data <- data.table(X = 1:5, Y = 11:15)
  path_csv <- tempfile()
  write.csv(data, path_csv, row.names = F)

  session$setInputs(
    primary_input = list(datapath = path_csv)
  )


  test_that("Can upload basic data", {
    expect_equal(data, gfpop_data$main_data)
  })

  # Try to upload invalid data, it shouldn't change
  data2 <- data.table(X = 11:15, Y = 1:5, Z = 21:25)
  path_csv2 <- tempfile()
  write.csv(data2, path_csv2, row.names = F)

  session$setInputs(
    primary_input = list(datapath = path_csv2)
  )

  test_that("Uploading data with invalid shape has no effect", {
    expect_equal(data, gfpop_data$main_data)
  })

  # Try to upload very invalid data, also shouldn't change
  data3 <- "helloworld"
  path3 <- tempfile()
  write(data3, path3)

  session$setInputs(
    primary_input = list(datapath = path3)
  )

  test_that("Uploading data with invalid type has no effect", {
    expect_equal(data, gfpop_data$main_data)
  })
})

# Generating
shiny::testServer(mod_home_server, {
  # With proper means, proper number of changepoints
  session$setInputs(
    meansChangepoints = "1,2,1,4",
    nChangepoints = 4,
    eChangepoints = 1,
    ndata = 100,
    type = "mean",
    typeChangepoints = "mean",
    sigma = 1, gammaChangepoints = 1
  )

  session$setInputs(genData = 0)

  test_that("gendata produces reasonable data", {
    expect_equal(dim(gfpop_data$main_data), c(100, 2))
  })


  data_save <- gfpop_data$main_data

  # Can re-generate data with more changepoints
  session$setInputs(
    nChangepoints = 5,
    meansChangepoints = "1,2,1,4,1"
  )

  session$setInputs(genData = 1)

  test_that("genData does change", {
    expect_false(isTRUE(all.equal(gfpop_data$main_data, data_save)))
  })

  data_save2 <- gfpop_data$main_data


  # With proper means, improper number of changepoints
  session$setInputs(
    nChangepoints = 1
  )

  session$setInputs(genData = 3)

  test_that("gendata does not change when nChangepoints is improper", {
    expect_equal(gfpop_data$main_data, data_save2)
  })


  # With improper means
  session$setInputs(
    meansChangepoints = 1e6,
    nChangepoints = 5,
    ndata = 100,
    type = "mean",
    sigma = 1, gamma = 1
  )
  session$setInputs(genData = 4)

  test_that("gendata does not change when means are improper", {
    expect_equal(gfpop_data$main_data, data_save2)
  })

  # With another invalid parameter
  session$setInputs(
    ndata = "hello",
    type = "mean",
    sigma = 1, gamma = 1
  )
  session$setInputs(genData = 5)

  test_that("gendata does not change when ndata or type are invalid", {
    expect_equal(gfpop_data$main_data, data_save2)
  })
})

# Upload graph
shiny::testServer(mod_home_server, {
  data <- gfpop::graph(type = "updown")
  path_csv <- tempfile()
  write.csv(data, path_csv, row.names = F)

  session$setInputs(
    constraint_graph = list(datapath = path_csv)
  )

  data2 <- data.table()
  path_csv2 <- tempfile()
  write.csv(data2, path_csv2, row.names = F)

  session$setInputs(
    constraint_graph = list(datapath = path_csv2)
  )
})

# mod_analysis_server.R, general -----------------------------------------------
test_that(
  "Server tests: mod_analysis_server",
  {
    expect_null(
      shiny::testServer(mod_analysis_server, {
        # Reset the graph first to test updateGraph()
        gfpop_data$graphdata <- NULL
        gfpop_data$graphdata_visNetwork <- NULL
        assert_that(
          "Graph data reset successfully",
          (is.null(gfpop_data$graphdata) && is.null(gfpop_data$graphdata_visNetwork))
        )

        print(input$updateGraph)
        print(gfpop_data)
        session$setInputs(pen = 15, graphType = "std", showNull = TRUE, updateGraph = 1)
        print(input$updateGraph)
        print(gfpop_data)
        print(gfpop_data$graphdata)
        assert_that(
          "graphdata is reasonable after  update_graph [analysis]",
          ncol(gfpop_data$graphdata) == 9,
          gfpop_data$graphdata
        )

        assert_that(
          "graphdata_visNetwork is reasonable after update_graph [analysis]",
          {
            testdata <- gfpop_data$graphdata_visNetwork
            test1 <- all(names(testdata) == c("nodes", "edges"))
            test2 <- ncol(testdata$nodes) == 9
            test3 <- ncol(testdata$edges) == 15
            test1 && test2 && test3
          },
          list(
            gfpop_data$graphdata_visNetwork,
            names(testdata), ncol(testdata$nodes),
            ncol(testdata$edges)
          )
        )

        session$setInputs(showNull = FALSE)
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

        if (interactive()) {
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
        }
        # Changepoint tests ------------------------------------------------------
        gfpop_data$main_data <- data.table(
          X = 1:100,
          Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1),
            c(1, 2, 1, 3, 1),
            sigma = 1
          )
        )

        session$setInputs(gfpopType = "mean")
        assert_that(
          "changepoints do not exist before init",
          !("changepoints" %in% names(gfpop_data)),
          list(gfpop_data)
        )

        initialize_changepoints()

        assert_that(
          "initalize_changepoints works",
          length(gfpop_data$changepoints) == 5,
          ""
        )

        # GraphOutput cell edit tests --------------------------------------------
        # Reset nodes and edges
        session$setInputs(pen = 15, graphType = "std", showNull = TRUE, updateGraph = 1)

        assert_that(
          "ensure penalty starts at 15",
          {
            all(gfpop_data$graphdata$penalty == c(0, 15)) &&
              all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 15))
          },
          list(
            gfpop_data$graphdata,
            gfpop_data$graphdata_visNetwork$edges
          )
        )

        # Run through the switch-a-roo once
        gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)

        session$setInputs(graphOutput_cell_edit = list(row = 2, col = 5, value = 20))



        assert_that(
          "Can edit penalty from graphOutput",
          {
            all(gfpop_data$graphdata$penalty == c(0, 20)) &&
              all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 20))
          },
          list(
            gfpop_data$graphdata,
            gfpop_data$graphdata_visNetwork$edges
          )
        )

        session$setInputs(graphOutput_visEdges_cell_edit = list(row = 2, col = 7, value = 100))

        assert_that(
          "Can edit penalty from graphOutput_visEdges",
          {
            all(gfpop_data$graphdata$penalty == c(0, 100)) &&
              all(gfpop_data$graphdata_visNetwork$edges$penalty == c(0, 100))
          },
          list(
            gfpop_data$graphdata,
            gfpop_data$graphdata_visNetwork$edges
          )
        )

        session$setInputs(graphOutput_visNodes_cell_edit = list(row = 1, col = 3, value = 100))
        assert_that(
          "Can edit nodes from graphoutput_visNodes",
          all(gfpop_data$graphdata_visNetwork$nodes$size == c(100)),
          list(gfpop_data$graphdata_visNetwork$nodes)
        )

        # Start/end node testing
        session$setInputs(
          setStart = "Std", setEnd = "Std",
          setStartEnd_button = 0
        )
        assert_that(
          "Can set a start and end nodes",
          gfpop_data$graphdata$type[3] == "start" &
            gfpop_data$graphdata$state1[3] == "Std" &
            gfpop_data$graphdata$type[4] == "end" &
            gfpop_data$graphdata$state1[4] == "Std",
          list(gfpop_data)
        )
      })
    )
  }
)

# Saving and loading -----------------------------------------------------------
shiny::testServer(mod_analysis_server, {
  updown_graph <- gfpop::graph(type = "updown")
  std_graph <- gfpop::graph(type = "std")

  gfpop_data$graphdata <- updown_graph
  session$setInputs(saveId = "test_save")
  session$setInputs(saveButton = 1)

  test_that("Basic saving of gfpop data works", {
    expect_equal(saved_analyses$saved_full[["test_save"]]$graphdata, updown_graph)
  })


  # Attempting to save with the same ID
  gfpop_data$graphdata <- std_graph
  session$setInputs(saveId = "test_save")
  session$setInputs(saveButton = 2)

  test_that("Saving duplicate IDs does not work", {
    expect_equal(
      saved_analyses$saved_full[["test_save"]]$graphdata, updown_graph
    )
  })

  # OK with a different ID, though
  session$setInputs(saveId = "test_save2")
  session$setInputs(saveButton = 3)

  test_that("Saving with non-duplicate IDs works", {
    expect_equal(
      saved_analyses$saved_full[["test_save2"]]$graphdata, std_graph,
    )
  })

  # Can save multiple values
  gfpop_data$base_plot <- plotly::plot_ly(x = ~ 1:5, y = ~ 11:15) %>% plotly::add_markers()

  session$setInputs(saveId = "test_save3")
  session$setInputs(saveButton = 4)

  test_that("Saving multiple parameters works", {
    expect_equal(
      saved_analyses$saved_full[["test_save3"]]$graphdata, std_graph,
    )
    expect_silent(
      is_a("plotly")(saved_analyses$saved_full[["test_save3"]]$base_plot)
    )
  })


  # Reset gfpop data
  session$setInputs(loadId = "test_save")
  session$setInputs(loadButton = 1)
  test_that("Can do a basic load of gfpop data", {
    expect_equal(gfpop_data$graphdata, updown_graph)
    expect_null(gfpop_data$base_plot)
  })

  session$setInputs(loadId = "test_save3")
  session$setInputs(loadButton = 2)
  test_that("Can load multiple values from a save", {
    expect_equal(gfpop_data$graphdata, std_graph)
    expect_silent(
      is_a("plotly")(gfpop_data$base_plot)
    )
  })
})

# Graph edits ------------------------------------------------------------------

# From visNetwork
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std", penalty = 15)

  gfpop_data$graphdata <- std_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(std_graph,
    edge_ids = c(
      "Std_Std_null",
      "Std_Std_std"
    )
  )

  session$setInputs(gfpopGraph_graphChange = event <- list(
    cmd = "editEdge",
    to = "Std", from = "Std",
    id = "Std_Std_std", type = "std", penalty = 200, parameter = 0,
    K = Inf, a = 0, min = "None", max = "None", hidden = "FALSE"
  ))

  test_that("gfpopGraph_graphChange triggers a visNetwork graph modification", {
    expect_equal(gfpop_data$graphdata_visNetwork$edges$penalty, c(0, 200))
  })

  test_that("gfpopGraph_graphChange also keeps the main gfpopdata graph up to date", {
    expect_equal(gfpop_data$graphdata$penalty, c(0, 200))
  })
})

# From the "add node" button
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std", penalty = 15)

  gfpop_data$graphdata <- std_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(std_graph,
    edge_ids = c(
      "Std_Std_null",
      "Std_Std_std"
    )
  )

  session$setInputs(
    addNode_id = "new_test_node",
    addNull = FALSE,
    addNode_button = 0
  )

  test_that("add_node button works as expected without addNull", {
    # Did not add any edges
    expect_equal(
      gfpop_data$graphdata,
      std_graph
    )

    # Did add a node
    expect_equal(
      gfpop_data$graphdata_visNetwork$nodes$id,
      c("Std", "new_test_node")
    )
  })

  session$setInputs(
    addNode_id = "new_test_node2",
    addNull = TRUE,
    addNode_button = 1
  )

  test_that("add_node button works as expected with addNull", {
    # Did add an edge
    expect_equal(
      gfpop_data$graphdata$state1,
      c("Std", "Std", "new_test_node2")
    )

    # Column names have now changed, since we converted from visNetwork
    expect_true("state1_id" %in% colnames(gfpop_data$graphdata) &
      "state2_id" %in% colnames(gfpop_data$graphdata))
  })

  # Try to add a new node with a duplicate ID
  session$setInputs(
    addNode_id = "new_test_node2",
    addNull = TRUE,
    addNode_button = 2
  )

  test_that("No change if you try to add a duplicate node id", {
    # Did add an edge
    expect_equal(
      gfpop_data$graphdata$state1,
      c("Std", "Std", "new_test_node2")
    )

    # Column names have now changed, since we converted from visNetwork
    expect_true("state1_id" %in% colnames(gfpop_data$graphdata) &
      "state2_id" %in% colnames(gfpop_data$graphdata))
  })

  session$setInputs(
    addNode_id = "",
    addNull = TRUE,
    addNode_button = 2
  )
  test_that("No change if you try to add an empty node id", {
    # Did add an edge
    expect_equal(
      gfpop_data$graphdata$state1,
      c("Std", "Std", "new_test_node2")
    )
  })
})

# From the "remove node" button
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std")
  updown_graph <- gfpop::graph(type = "updown")

  gfpop_data$graphdata <- std_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(std_graph,
    edge_ids = c(
      "Std_Std_null",
      "Std_Std_std"
    )
  )

  session$setInputs(
    setRemoveNode = "Std",
    removeNode_button = 0
  )

  test_that("Can remove all nodes, also removes all edges", {
    expect_equal(nrow(gfpop_data$graphdata), 0)
    expect_equal(nrow(gfpop_data$graphdata_visNetwork$edges), 0)
    expect_equal(nrow(gfpop_data$graphdata_visNetwork$nodes), 0)
  })


  gfpop_data$graphdata <- updown_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(updown_graph)

  session$setInputs(
    setRemoveNode = "Dw",
    removeNode_button = 1
  )

  test_that("Removing one node removes all edges associated with that node", {
    expect_equal(gfpop_data$graphdata_visNetwork$nodes$id, c("Up"))
    expect_equal(gfpop_data$graphdata_visNetwork$edges$id, c("Up_Up"))
    expect_equal(gfpop_data$graphdata$state1, c("Up"))
  })
})

# From the "addEdge" button
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std")
  updown_graph <- gfpop::graph(type = "updown")

  gfpop_data$graphdata <- updown_graph[3:4, ]
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata)

  session$setInputs(
    labels = c(),
    addEdge_to = "Up",
    addEdge_from = "Up",
    addEdge_type = "null",
    addEdge_parameter = 23,
    addEdge_penalty = 11
  )

  session$setInputs(addEdge_button = 1)

  test_that("addEdge button can add an appropriate edge", {
    expect_equal(gfpop_data$graphdata$state1, c("Dw", "Up", "Up"))
    expect_equal(gfpop_data$graphdata$state2, c("Up", "Dw", "Up"))
    expect_equal(
      gfpop_data$graphdata_visNetwork$edges$id,
      c("Dw_Up", "Up_Dw", "Up_Up_null")
    )
  })
})

# Start and end nodes ----------------------------------------------------------
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std", penalty = 15)

  gfpop_data$graphdata <- std_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(std_graph,
    edge_ids = c(
      "Std_Std_null",
      "Std_Std_std"
    )
  )

  session$setInputs(
    setStart = "Std", setEnd = "Std",
    setStartEnd_button = 0,
    showNull = FALSE,
    label_columns = c()
  )

  test_that("Basic setting start and end works as expected", {
    expect_equal(gfpop_data$graphdata$type, c(
      "null", "std",
      "start", "end"
    ))
    expect_equal(gfpop_data$graphdata$state1, rep("Std", 4))
    expect_equal(gfpop_data$graphdata_visNetwork$nodes$shape, "star")
  })

  session$setInputs(
    setStart = "Std", setEnd = "N/A",
    setStartEnd_button = 0,
    showNull = FALSE,
    label_columns = c()
  )

  test_that("Re-setting the ending node works as expected", {
    expect_equal(gfpop_data$graphdata$type, c(
      "null", "std",
      "start"
    ))
    expect_equal(gfpop_data$graphdata$state1, rep("Std", 3))
    expect_equal(gfpop_data$graphdata_visNetwork$nodes$shape, "triangle")
  })

  session$setInputs(
    setStart = "N/A", setEnd = "N/A",
    setStartEnd_button = 0,
    showNull = FALSE,
    label_columns = c()
  )

  test_that("Re-setting the ending node works as expected", {
    expect_equal(gfpop_data$graphdata$type, c("null", "std"))
    expect_equal(gfpop_data$graphdata$state1, rep("Std", 2))
    expect_equal(gfpop_data$graphdata_visNetwork$nodes$shape, "dot")
  })
})

# Crosstalk --------------------------------------------------------------------

# input$gfpopGraph_highlight_color_id
shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std")
  updown_graph <- gfpop::graph(type = "updown")

  gfpop_data$graphdata <- updown_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata)

  gfpop_data$changepoint_annotations_list <- add_changepoints(
    plotly::plot_ly(),
    data.table::data.table(1:20, c(1:10, 101:110)),
    gfpop::gfpop(c(1:10, 101:110), mygraph = gfpop::graph(type = "updown", penalty = 10))
  )

  session$setInputs(
    crosstalk = TRUE
  )

  session$setInputs(
    gfpopGraph_highlight_color_id = "Up"
  )

  test_that("Highlighting segments correctly keeps track of highlighted", {
    expect_equal(selected$segments, c("Up"))
  })

  session$setInputs(
    gfpopGraph_highlight_color_id = "Dw"
  )

  test_that("Highlighting segments correctly un-highlights previously-highlighted segments", {
    expect_equal(selected$segments, c("Dw"))
  })
})

# Main gfpop running -----------------------------------------------------------

shiny::testServer(mod_analysis_server, {
  std_graph <- gfpop::graph(type = "std")
  updown_graph <- gfpop::graph(type = "updown")

  gfpop_data$graphdata <- updown_graph
  gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(gfpop_data$graphdata)

  gfpop_data$base_plot <- plotly::plot_ly()
  gfpop_data$main_data <- data.table::data.table(X = 1:20, Y = c(1:10, 101:110))
  gfpop_data$changepoints <- gfpop::gfpop(c(1:10, 101:110),
    mygraph = gfpop::graph(
      type = "updown",
      penalty = 10
    )
  )
  session$setInputs(
    runGfpop = 0
  )

  test_that("runGfpop successfully produces the correct items", {
    expect_equal(
      names(gfpop_data$changepoint_annotations_list),
      c(
        "plot", "changepoint_annotations",
        "changepoint_annotations_regions"
      )
    )
  })

  test_that("runGfpop creates the right sort of changepoint plot", {
    expect_equal(
      class(gfpop_data$changepoint_plot),
      c("plotly", "htmlwidget")
    )
  })
})

shiny::testServer(mod_analysis_server, {
  gfpop_data$changepoints <- "Something"
  gfpop_data$changepoint_plot <- "Also something"
  gfpop_data$base_plot <- "The base plot"

  session$setInputs(
    clsCp = 0
  )

  test_that("clsCp successfully clears changepoints data", {
    expect_null(gfpop_data$changepoints)
    expect_equal(gfpop_data$changepoint_plot, "The base plot")
  })
})
