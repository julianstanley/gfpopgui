library(RSelenium)
library(testthat)

# Initialization steps ---------------------------------------------------------

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
Sys.sleep(4)

# Local Tests ------------------------------------------------------------------
# Connect to the local app
remDr$navigate(url = "http://127.0.0.1:15123")

test_that("can connect to app, local", {
  skip_on_ci()
  appTitle <- remDr$getTitle()[[1]]
  remDr$screenshot(file = "screenshots/test_connect_local.png")
  expect_equal(appTitle, "gfpopgui")
})

# Remote Tests -----------------------------------------------------------------
# Connect to the remote app, wait a second for load
remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
Sys.sleep(2)

test_that("can connect to app, remote", {
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
})

# Cleanup steps ----------------------------------------------------------------

# Close selenium server
remDr$close()
# Kill app
clean.kill.app <- system("lsof -i:15123 -t | xargs kill", intern = TRUE)


