test_that("generate_changepoint produces the correct column names and dimensions", {
  data <- gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
                               c(1, 2, 1, 3, 1), sigma = 1)
  graph <- gfpop::graph(type = "std")
  result <- generate_changepoint(data, graph)
  
  expect_equal(dim(result), c(10, 3))
  expect_equal(colnames(result), c("changepoint", "changepoint_end", "y"))
})

test_that("associated changepoint calculation produces correct column names and dimensions", {
  data <- gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
                               c(1, 2, 1, 3, 1), sigma = 1)
  graph <- gfpop::graph(type = "std")
  changepoint_data <- generate_changepoint(data, graph)
  result <- get_associated_changepoint(2, changepoint_data)
  
  expect_equal(dim(result), c(1, 3))
  expect_equal(colnames(result), c("changepoint", "changepoint_end", "y"))
})

test_that("associated changepoint calculation finds correct changepoints", {
  set.seed(100)
  data <- gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
                               c(1, 2, 1, 3, 1), sigma = 1)
  graph <- gfpop::graph(type = "std")
  changepoint_data <- generate_changepoint(data, graph)
  
  result <- get_associated_changepoint(2, changepoint_data)
  expect_lt(result$y - 2.131531, 0.0001)
  
  result <- get_associated_changepoint(2.001, changepoint_data)
  expect_lt(result$y - 1.921083, 0.0001)
  
  result <- get_associated_changepoint(2.5, changepoint_data)
  expect_lt(result$y - 1.921083, 0.0001)
  
  result <- get_associated_changepoint(3, changepoint_data)
  expect_lt(result$y - 1.921083, 0.0001)
  
  result <- get_associated_changepoint(3.001, changepoint_data)
  expect_lt(result$y - 1.886785, 0.0001)
  
})

test_that("annotate_data_with_changepoint gives correct column names and dimensions", {
  data <- data.frame(X = 1:10, Y = gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
                                                        c(1, 2, 1, 3, 1), sigma = 1))
  graph <- gfpop::graph(type = "std")
  changepoint_data <- generate_changepoint(data$Y, graph)
  result <- annotate_data_with_changepoint(data, changepoint_data)
  
  expect_equal(dim(result), c(10, 6))
  expect_equal(colnames(result), c("X", "Y", "changepoint", 
                                   "changepoint_end", "y", "CP_Data"))
})