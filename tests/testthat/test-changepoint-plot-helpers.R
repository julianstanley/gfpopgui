library(vdiffr)
library(plotly)

test_that("add_changepoints produces reasonable plot asthetics",{
  data <- data.frame(X = seq(5, 870, length = 100), Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1), c(1,2,1,3,1), sigma = 1))
  myplot <- plot_ly(data, x = ~X, y = ~Y, hoverinfo = 'none') %>% add_markers()
  mygraph <- gfpop::graph(type = "updown", penalty = 15)
  changepoint_data <- gfpop::gfpop(data = data$Y, mygraph = mygraph,
                                   type = "mean")
  test_plot <- add_changepoints(myplot, data, changepoint_data)[["plot"]]

  expect_equal(test_plot$x$attrs[[2]][8]$mode, "markers")
  expect_equal(test_plot$x$attrs[[3]][8]$mode, "lines")
  expect_equal(test_plot$x$attrs[[4]][8]$mode, "lines")
  
  expect_equal(test_plot$x$attrs[[3]]$line$width, 7)
  expect_equal(test_plot$x$attrs[[4]]$line$width, 7)
  
  expect_equal(test_plot$x$attrs[[3]]$connectgaps, FALSE)
  expect_equal(test_plot$x$attrs[[4]]$connectgaps, FALSE)
})
