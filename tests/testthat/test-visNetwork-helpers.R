test_that("graphdf_to_visNetwork generate a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(typeof(visNetwork_list), "list")
})

test_that("graphdf_to_visNetwork has 'nodes' and 'edges' as labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(labels(visNetwork_list), c("nodes", "edges"))
})

test_that("visNetwork function generates a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(typeof(generate_visNetwork(visNetwork_list)), "list")
})

test_that("visNetwork function generates a list with certain labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  associated_visNetwork <- generate_visNetwork(visNetwork_list)
  
  expect_equal(labels(associated_visNetwork), 
               c("x", "width", "height", "sizingPolicy", "dependencies", 
                 "elementId", "preRenderHook", "jsHooks"))
})
