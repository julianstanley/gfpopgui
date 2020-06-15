# NAtoNone
test_that("NAtoNone doesn't affect an empty vector", {
  expect_equal(NAtoNone(c()), list())
})

test_that("NAtoNone converts NAs to the string 'None'", {
  expect_equal(NAtoNone(rep(NA, 3)), as.list(rep("None", 3)))
})

test_that("NAtoNone keeps non-NA values constant", {
  expect_equal(NAtoNone(c(NA, 3, NA)), list("None", 3, "None"))
})

test_that("NAtoNone doesn't affect 'None' strings", {
  expect_equal(NAtoNone(list(NA, 3, NA, "None")), list("None", 3, "None", "None"))
})

# NonetoNA
test_that("NonetoNA doesn't affect an empty vector", {
  expect_equal(NonetoNA(c()), list())
})

test_that("NonetoNA converts the string 'None' to NA", {
  expect_equal(NonetoNA(rep("None", 3)), as.list(rep(NA, 3)))
})

test_that("NonetoNA keeps non-None values constant", {
  expect_equal(NonetoNA(c("None", 'test', "None")), list(NA, 'test', NA))
})

test_that("NonetoNA doesn't affect existing NA", {
  expect_equal(NonetoNA(c(NA, 'test', NA, "None")), list(NA, 'test', NA, NA))
})

test_that("NonetoNA is the inverse of NAtoNone", {
  result <- NonetoNA(NAtoNone(c(1,2,4,2,NA,2,4,NA)))
  expect_equal(NonetoNA(NAtoNone(c(1,2,4,2,NA,2,4,NA))), list(1,2,4,2,NA,2,4,NA))
})

# graphdf_to_visNetwork
test_that("graphdf_to_visNetwork generate a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(typeof(visNetwork_list), "list")
})

test_that("graphdf_to_visNetwork has 'nodes' and 'edges' as labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(labels(visNetwork_list), c("nodes", "edges"))
})

#visNetwork_to_graphdf
test_that("visNetwork_to_graphdf is inverse of graphdf_to_visNetwork", {
  expect_equal(visNetwork_to_graphdf(
    graphdf_to_visNetwork(gfpop::graph(type = "std"))), gfpop::graph(type = "std"))
})

# generate_visNetwork
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
