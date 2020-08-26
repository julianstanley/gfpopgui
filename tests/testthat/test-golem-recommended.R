test_that("app ui", {
  skip_if_not(interactive())
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
})

test_that("app server", {
  skip_if_not(interactive())
  server <- app_server
  expect_is(server, "function")
})

# Configure this test to fit your need
test_that(
  "app launches",{
    skip_if_not(interactive())
    golem::expect_running(sleep = 5)
  }
)