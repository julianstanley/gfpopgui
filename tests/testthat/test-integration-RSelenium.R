library(RSelenium)
library(testthat)

user <- "julianstanley"
pass <- "0135a1f4-8837-4202-a428-d58151760e6b"
port <- 80
ip <- paste0(user, ":", pass, "@ondemand.saucelabs.com")
rdBrowser <- "chrome"
version <- "latest"
platform <- "Windows 10"

extraCapabilities <- list(
  name = "Main test-integration",
  username = user,
  accessKey = pass,
  tags = list("RSelenium-vignette", "OS/Browsers-vignette"),
  "screen-resolution" = "1920x1080"
)

remDr <- remoteDriver$new(
  remoteServerAddr = ip,
  port = port,
  browserName = rdBrowser,
  version = version,
  platform = platform,
  extraCapabilities = extraCapabilities
)

remDr$open(silent = T)
remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
Sys.sleep(5)

test_that("can connect to app, remote", {
  # Connect to the remote app, wait a second for load
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "gfpopgui")
})

test_that("the generate data button works", {
  # Select entry from DataTable 0 to make sure it exists
  webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_0']/tbody/tr/td[2]")
  expect_equal(webElem$getElementAttribute("innerHTML")[[1]], "Std")
  expect_error(remDr$findElement("xpath", "//table[@id='DataTables_Table_1']/tbody/tr/td[2]"))
  remDr$findElement("id", "home_ui_1-genData")$clickElement()
  webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_1']/tbody/tr/td[2]")
  expect_equal(webElem$getElementAttribute("innerHTML")[[1]], "1")
})

remDr$close()
