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

wait_for_title <- function(remDr, seconds, title_expected) {
  count <- 0
  while(count < seconds) {
    appTitle <- remDr$getTitle()[[1]]
    if(appTitle == title_expected) {
      break
    }
    message(paste0("Waited ", count, " seconds. App isn't loaded yet, waiting
                   for another second."))
    Sys.sleep(1)
    count <- count + 1
  }
  
  return(appTitle)
}

test_that("can connect to app", {
  remDr$open(silent = T)
  remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
  remDr$setImplicitWaitTimeout(milliseconds = 5000)
  appTitle <- wait_for_title(remDr, 10, "gfpopgui")
  result <- if(appTitle == "gfpopgui") "true" else "false"
  submit_job_info(remDr, buildName, name = "can connect to app",
                                    result = result)
  expect_equal(appTitle, "gfpopgui")
  remDr$close()
})

test_that("the generate data button works", {
  remDr$open(silent = T)
  remDr$navigate(url = "http://julianstanley.shinyapps.io/gfpopgui")
  remDr$setImplicitWaitTimeout(milliseconds = 5000)
  # Wait for the app to load
  appTitle <- wait_for_title(remDr, 10, "gfpopgui")
  if(appTitle != "gfpopgui") {
    warning("Title is not 'gfpopgui'. Did the application load successfully?")
  }
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
