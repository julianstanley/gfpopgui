test_that("app ui", {
  ui_test <- app_ui()
  golem::expect_shinytaglist(ui_test)
})

test_that("app server", {
  server_test <- app_server
  expect_is(server_test, "function")
})

# Configure this test to fit your need
test_that(
  "app launches",{
    skip_if_not(interactive())
    golem::expect_running(sleep = 5)
  }
)