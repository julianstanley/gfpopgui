library(data.table)
library(gfpop)

test_that("select_graph_columns works as expected", {
  graph <- gfpop::graph(type = "std")
  newgraph <- graph
  newgraph$test <- c(1, 2)

  expect_equal(
    all.equal(graph, newgraph),
    "Length mismatch: comparison on first 9 components"
  )
  expect_true(all.equal(data.table(graph), select_graph_columns(newgraph)))
})

test_that("format_edge works as expected", {
  row <- gfpop::graph(type = "updown")[1, ]

  expect_equal(format_edge(row), paste0(
    "gfpop::Edge(state1 = 'Dw', state2 = 'Dw',",
    " type = 'null', gap = 1, penalty = 0, K = Inf, a = 0)"
  ))
})

test_that("format_edge produces code that exactly replicates a given edge", {
  row <- gfpop::graph(type = "updown")[1, ]
  row_text <- format_edge(row)
  row_prime <- eval(parse(text = row_text))

  # Cast min and max rows to numeric, remove with resolution of gfpop
  # issue #14
  row_prime[c("min", "max")] <- as.numeric(row_prime[c("min", "max")])
  row_prime <- gfpop::graph(row_prime)

  expect_equal(row, row_prime)
})

test_that("graph_to_R_code works as expected", {
  graph <- gfpop::graph(type = "std")
  graph_text <- graph_to_R_code(graph)
  expect_equal(
    graph_text,
    "gfpop::graph(
    gfpop::Edge(state1 = 'Std', state2 = 'Std', type = 'null', gap = 1, penalty = 0, K = Inf, a = 0),
    gfpop::Edge(state1 = 'Std', state2 = 'Std', type = 'std', gap = 0, penalty = 0, K = Inf, a = 0)
)"
  )
})

test_that("graph_to_R_code produces code that exactly replicates a given graph", {
  graph <- gfpop::graph(type = "std")
  graph_text <- graph_to_R_code(graph)
  graph_prime <- eval(parse(text = graph_text))

  # Cast min and max rows to numeric, remove with resolution of gfpop
  # issue #14
  graph_prime[c("min", "max")] <- apply(graph_prime[c("min", "max")], 2, as.numeric)
  graph_prime <- gfpop::graph(graph_prime)

  expect_equal(graph, graph_prime)
})

test_that("graph_to_R_code fails if it has the incorrect column names", {
  graph <- gfpop::graph(type = "std")
  graph[["dummy_col"]] <- 1

  expect_error(graph_to_R_code(graph))

  graph <- gfpop::graph(type = "std")
  graph[["state1"]] <- NULL

  expect_error(graph_to_R_code(graph))
})

test_that("graph_to_R_code can handle a starting node", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(start = 0)
  )

  graph_text <- graph_to_R_code(graph)
  graph_prime <- eval(parse(text = graph_text))

  # Cast min and max rows to numeric, remove with resolution of gfpop
  # issue #14
  graph_prime[c("min", "max")] <- apply(graph_prime[c("min", "max")], 2, as.numeric)
  graph_prime <- gfpop::graph(graph_prime)

  expect_equal(graph, graph_prime)
})

test_that("graph_to_R_code can handle an ending node", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(end = 2)
  )

  graph_text <- graph_to_R_code(graph)
  graph_prime <- eval(parse(text = graph_text))

  # Cast min and max rows to numeric, remove with resolution of gfpop
  # issue #14
  graph_prime[c("min", "max")] <- apply(graph_prime[c("min", "max")], 2, as.numeric)
  graph_prime <- gfpop::graph(graph_prime)

  expect_equal(graph, graph_prime)
})

test_that("graph_to_R_code can handle a start and end node at once", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(start = 0, end = 2)
  )

  graph_text <- graph_to_R_code(graph)
  graph_prime <- eval(parse(text = graph_text))

  # Cast min and max rows to numeric, remove with resolution of gfpop
  # issue #14
  graph_prime[c("min", "max")] <- apply(graph_prime[c("min", "max")], 2, as.numeric)
  graph_prime <- gfpop::graph(graph_prime)

  expect_equal(graph, graph_prime)
})
