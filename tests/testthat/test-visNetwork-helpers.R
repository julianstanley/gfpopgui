generate_visNetwork_list <- function() {
  graphdf_to_visNetwork(gfpop::graph(type = "std"))
}

test_that("graphdf_to_visNetwork generate a list", {
  expect_type(generate_visNetwork_list(), "list")
})

test_that("graphdf_to_visNetwork has 'nodes' and 'edges' as labels", {
  expect_equal(labels(generate_visNetwork_list()), c("nodes", "edges"))
})

test_that("visNetwork function generates a list", {
  expect_type(generate_visNetwork(generate_visNetwork_list()), "list")
})

test_that("visNetwork function generates a list with certain labels", {
  expect_equal(labels(generate_visNetwork(generate_visNetwork_list())), 
               c("x", "width", "height", "sizingPolicy", "dependencies", 
                 "elementId", "preRenderHook", "jsHooks"))
})
