library(vdiffr)

test_that("changepoint plot is visually consistent", {
  set.seed(100)
  data <- data.frame(X = 1:100, Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1), 
                                                         c(1, 2, 1, 3, 1), sigma = 1))
  graph <- gfpop::graph(type = "std", penalty = 15)
  changepoint_data <- generate_changepoint(data$Y, graph)
  annotated_data <- annotate_data_with_changepoint(data, changepoint_data)
  ggplot(annotated_data) + geom_point(aes(x = X, y = Y))
  vdiffr::expect_doppelganger("Example changepoint", plot_changepoint(annotated_data))
})