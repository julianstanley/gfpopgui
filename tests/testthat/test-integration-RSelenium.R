library(RSelenium)
library(testthat)

# Initialization steps ---------------------------------------------------------

# For local debug, change to true!
SCREENSHOT <- FALSE
RUN_DOCKER <- FALSE

# To disable remote shinyapps.io checking, change to false!
REMOTE <- FALSE

# Start a local selenium server via docker (local only)
if(RUN_DOCKER) {
  system("docker run -d --net=host selenium/standalone-firefox&")
}

# Open port to RSelenium server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browser = "firefox") 
remDr$open(silent = TRUE)

# Kill the shiny application, if it's running
# Use port=15123 for testing
init.kill.app <- system("lsof -i:15123 -t | xargs kill", 
                        ignore.stdout = TRUE,
                        ignore.stderr = TRUE)

# Start the shiny application
system("${R_HOME}/bin/Rscript -e 'library(gfpopgui);options(shiny.port = 15123);run_app()' &", 
       ignore.stdout = TRUE,
       ignore.stderr = TRUE)

# Give the system a few seconds to load app
Sys.sleep(10)

# Local Tests ------------------------------------------------------------------

# Connect to the local app, wait for a second for load
remDr$navigate(url = "http://127.0.0.1:15123")

testthat::try_again(5, test_that("can connect to app, local", {
  appTitle <- remDr$getTitle()[[1]]
  # Screenshot debug, local only:
  if(SCREENSHOT) {
    remDr$screenshot(file = "screenshots/test_connect_local.png")
  }
  expect_equal(appTitle, "gfpopgui")
}))

# Remote Tests -----------------------------------------------------------------
testthat::try_again(5, test_that("can connect to app, remote", {
  skip_on_ci()
  if(!REMOTE) {
    testthat::skip("Remote testing is disabled")
  }
  # Connect to the remote app, wait a second for load
  remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
  Sys.sleep(2)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
}))

# Cleanup steps ----------------------------------------------------------------

# Close selenium server
remDr$close()

# Kill app
clean.kill.app <- system("lsof -i:15123 -t | xargs kill", intern = TRUE)

