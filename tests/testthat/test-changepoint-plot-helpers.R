library(vdiffr)
library(plotly)
library(gfpop)

test_that("make_changepoint works as expected", {
  dt <- make_changepoint(
    parameter_x = 5, region_y = 1:10,
    new_state = "std", previous_state = "up"
  )

  expect_equal(nrow(dt), 11)
})

test_that("make_changepoint's add_na works as expected", {
  dt_t <- make_changepoint(
    parameter_x = 5, region_y = 1:10,
    new_state = "std", previous_state = "up", add_na = TRUE
  )
  dt_f <- make_changepoint(
    parameter_x = 5, region_y = 1:10,
    new_state = "std", previous_state = "up", add_na = FALSE
  )

  expect_equal(nrow(dt_f), 10)
  expect_equal(dt_t[1:10, ], dt_f)
})

test_that("make_region works as expected", {
  dt <- make_region(region_x = 1:10, parameter_y = 5, state = "up")

  expect_equal(nrow(dt), 11)
})

test_that("make_changepoint's add_na works as expected", {
  dt_t <- make_region(region_x = 1:10, parameter_y = 5, state = "up")
  dt_f <- make_region(region_x = 1:10, parameter_y = 5, state = "up", add_na = FALSE)


  expect_equal(nrow(dt_f), 10)
  expect_equal(dt_t[1:10, ], dt_f)
})

test_that("add_changepoints works for data with only one changeregion", {
  data <- data.frame(
    x = 1:100,
    y = gfpop::dataGenerator(100, c(1), c(1), sigma = 1)
  )
  mygraph <- gfpop::graph(type = "updown", penalty = 15)
  changepoint_data <- gfpop(data = data$y, mygraph = mygraph)
  added_changepoints <- add_changepoints(
    plotly_obj = plot_ly(), data,
    changepoint_data = changepoint_data
  )

  expect_equal(dim(added_changepoints$changepoint_annotations), c(0, 0))
  expect_equal(dim(added_changepoints$changepoint_annotations_regions), c(1000, 4))
})

test_that("add_changepoints works for data with many changeregions", {
  data <- data.frame(
    x = 1:10000,
    y = gfpop::dataGenerator(10000, seq(0.01, 1, length.out = 40), rep(c(1, 5), 20), sigma = 0.01)
  )
  mygraph <- gfpop::graph(type = "updown", penalty = 15)
  changepoint_data <- gfpop(data = data$y, mygraph = mygraph)
  added_changepoints <- add_changepoints(
    plotly_obj = plot_ly(), data,
    changepoint_data = changepoint_data
  )

  # There should be 3 rows for each changepoint, except the first, which
  # should have 0. So, for 40 changepoints, there should be 117
  expect_equal(dim(added_changepoints$changepoint_annotations), c(117, 4))

  # There should be four rows for each changeregion. So, 160 total.
  expect_equal(dim(added_changepoints$changepoint_annotations_regions), c(1064, 4))
})


test_that("add_changepoints produces reasonable plot asthetics", {
  data <- data.frame(
    X = seq(5, 870, length = 100),
    Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1), c(1, 2, 1, 3, 1), sigma = 1)
  )
  myplot <- plot_ly(data, x = ~X, y = ~Y, hoverinfo = "none") %>% add_markers()
  mygraph <- gfpop::graph(type = "updown", penalty = 15)
  changepoint_data <- gfpop::gfpop(
    data = data$Y, mygraph = mygraph,
    type = "mean"
  )
  test_plot <- add_changepoints(myplot, data, changepoint_data)[["plot"]]

  expect_equal(test_plot$x$attrs[[2]][8]$mode, "markers")
  expect_equal(test_plot$x$attrs[[3]][8]$mode, "lines")
  expect_equal(test_plot$x$attrs[[4]][8]$mode, "lines")

  expect_equal(test_plot$x$attrs[[3]]$line$width, 7)
  expect_equal(test_plot$x$attrs[[4]]$line$width, 7)

  expect_equal(test_plot$x$attrs[[3]]$connectgaps, FALSE)
  expect_equal(test_plot$x$attrs[[4]]$connectgaps, FALSE)
})
