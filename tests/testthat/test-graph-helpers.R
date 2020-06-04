test_that("select_graph_columns does its job", {
  graph <- gfpop::graph(type = "std")
  newgraph <- graph
  newgraph$test <- c(1,2)
  
  expect_failure(expect_equal(graph, newgraph))
  expect_equal(graph, select_graph_columns(newgraph))
})