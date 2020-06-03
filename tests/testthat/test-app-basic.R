library(RSelenium)
library(testthat)

# Open port to RSelenium server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browser = "firefox") 
remDr$open(silent = TRUE)

# Start the app
test_that("can connect to app, local", {
  skip_on_ci()
  remDr$navigate(url = "http://127.0.0.1:11616")
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
})

test_that("can connect to app, remote", {
  skip_on_travis()
  remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
})

remDr$close()

