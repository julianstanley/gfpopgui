test_that("select_graph_columns does its job", {
  graph <- gfpop::graph(type = "std")
  newgraph <- graph
  newgraph$test <- c(1,2)
  
  expect_equal(all.equal(graph, newgraph), 
               "Length mismatch: comparison on first 9 components")
  expect_true(all.equal(graph, select_graph_columns(newgraph)))
})