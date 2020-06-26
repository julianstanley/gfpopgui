if (RUN_SELENIUM | (identical(Sys.getenv("TRAVIS"), "true"))) {

library(RSelenium)
library(testthat)

user <- Sys.getenv("SAUCE_USERNAME")
pass <- Sys.getenv("SAUCE_SECRET_KEY")
port <- 80
ip <- paste0(user, ":", pass, "@ondemand.saucelabs.com")
rdBrowser <- "chrome"
version <- "latest"
platform <- "Windows 10"

if (identical(Sys.getenv("TRAVIS"), "true")) {
  extraCapabilities <- list(
    name = "Main test-integration",
    username = user,
    accessKey = pass,
    tags = list("R", "Shiny"),
    "screen-resolution" = "1920x1080",
    'tunnel-identifier' = Sys.getenv("TRAVIS_JOB_NUMBER")
  )
} else {
  print("Running integration tests on non-travis, please run SAUCECONNECT
        for port 80 and gfpopgui on port 3000.")
  extraCapabilities <- list(
    name = "Main test-integration",
    username = user,
    accessKey = pass,
    tags = list("R", "Shiny"),
    "screen-resolution" = "1920x1080")
}


remDr <- remoteDriver$new(
  remoteServerAddr = ip,
  port = port,
  browserName = rdBrowser,
  version = version,
  platform = platform,
  extraCapabilities = extraCapabilities
)

appURL <- "http://localhost:3000"

# Set build
buildName <- paste0("build-gfpopgui-main_",
                    format(Sys.time(), "%m-%d-%Y_%s"))

submit_job_info <- function(remDr, build, name, result) {
  if (!(result %in% c("passed", "failed", "true", "false"))) {
    stop("Invalid result. Please use: passed, failed, true, or false")
  }

  remDr$executeScript(paste0("sauce:job-build=", build))
  remDr$executeScript(paste0("sauce:job-name=", name))
  remDr$executeScript(paste0("sauce:job-result=", result))

}

is.bad <- function(code) {
  isTRUE(tryCatch(code,
                  error = function(c) TRUE,
                  warning = function(c) TRUE
  ))
}

test_that("can connect to app", {
  remDr$open(silent = T)
  remDr$navigate(url = appURL)
  remDr$setImplicitWaitTimeout(milliseconds = 5000)
  appTitle <- remDr$getTitle()[[1]]
  result <- if(appTitle == "gfpopgui") "true" else "false"
  submit_job_info(remDr, buildName, name = "can connect to app",
                                    result = result)
  expect_equal(appTitle, "gfpopgui")
  remDr$close()
})

test_that("the generate data button works", {
  remDr$open(silent = T)
  remDr$navigate(url = appURL)
  remDr$setImplicitWaitTimeout(milliseconds = 5000)
  # Select entry from DataTable 0 to make sure it exists
  webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_0']/tbody/tr/td[2]")
  result1 <- webElem$getElementAttribute("innerHTML")[[1]] == "Std"
  expect_equal(webElem$getElementAttribute("innerHTML")[[1]], "Std")

  expect_error(remDr$findElement("xpath", "//table[@id='DataTables_Table_1']/tbody/tr/td[2]"))
  remDr$findElement("id", "home_ui_1-genData")$clickElement()
  webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_1']/tbody/tr/td[2]")
  result2 <- webElem$getElementAttribute("innerHTML")[[1]] == "1"
  expect_equal(webElem$getElementAttribute("innerHTML")[[1]], "1")

  result <- if(result1 & result2) "true" else "false"
  submit_job_info(remDr, buildName, name = "the generate data button works",
                                    result = result)
  remDr$close()
})
}