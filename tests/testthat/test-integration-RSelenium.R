library(RSelenium)
library(testthat)

# Initialization steps ---------------------------------------------------------
user <- "julianstanley"
pass <- "0135a1f4-8837-4202-a428-d58151760e6b"
port <- 80
ip <- paste0(user, ":", pass, "@ondemand.saucelabs.com")
rdBrowser <- "chrome"
version <- "latest"
platform <- "Windows 10"

extraCapabilities <- list(
  name = "Main test-integration, gfpopgui",
  username = user,
  accessKey = pass,
  tags = list("RSelenium-vignette", "OS/Browsers-vignette")
)

remDr <- remoteDriver$new(
  remoteServerAddr = ip,
  port = port,
  browserName = rdBrowser,
  version = version,
  platform = platform,
  extraCapabilities = extraCapabilities
)
remDr$open()

test_that("can connect to app, remote", {
  # Connect to the remote app, wait a second for load
  remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
})

remDr$close()