library(RSelenium)
library(testthat)
enable_integration_tests <- FALSE
if (enable_integration_tests) {
  browsers_to_test <- list(
    list(rdBrowser = "chrome", version = "latest", 
         platform = "Windows 10", screen_resolution = "2560x1600"),
    list(rdBrowser = "chrome", version = "latest", 
         platform = "macOS 10.15", screen_resolution = "2048x1536")
  )
  
  # Set build
  buildName <- paste0(
    "build-gfpopgui-main_",
    format(Sys.time(), "%m-%d-%Y_%s")
  )
  
  for(browser in browsers_to_test) {

  user <- Sys.getenv("SAUCE_USERNAME")
  pass <- Sys.getenv("SAUCE_SECRET_KEY")
  port <- 80
  ip <- paste0(user, ":", pass, "@ondemand.saucelabs.com")
  rdBrowser <- browser[["rdBrowser"]]
  version <- browser[["version"]]
  platform <- browser[["platform"]]

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    extraCapabilities <- list(
      name = "Main test-integration",
      username = user,
      accessKey = pass,
      tags = list("R", "Shiny"),
      "screen-resolution" = browser[["screen_resolution"]],
      'tunnel-identifier' = Sys.getenv("TRAVIS_JOB_NUMBER")
    )
  } else {
    print("Running integration tests on non-travis, please run SAUCECONNECT
        for port 80 and gfpopgui on port 3000.")
    extraCapabilities <- list(
      name = "Main test-integration",
      username = user,
      accessKey = pass,
      tags = list("R", "Shiny"))
  }
  
  if(!is.null(browser[["screen_resolution"]])) {
    extraCapabilities$`screen-resolution` <- browser[["screen_resolution"]]
  }

  remDr <- remoteDriver$new(
    remoteServerAddr = ip,
    port = port,
    browserName = rdBrowser,
    version = version,
    platform = platform,
    extraCapabilities = extraCapabilities
  )
  
  appURL <- "http://julian.local:3000"

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
    while (count < seconds) {
      appTitle <- remDr$getTitle()[[1]]
      if (appTitle == title_expected) {
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
    remDr$navigate(url = appURL)
    remDr$setImplicitWaitTimeout(milliseconds = 5000)
    appTitle <- wait_for_title(remDr, 10, "gfpopgui")
    result <- if (appTitle == "gfpopgui") "true" else "false"
    submit_job_info(remDr, buildName,
      name = "can connect to app",
      result = result
    )
    expect_equal(appTitle, "gfpopgui")
    remDr$close()
  })

  test_that("the generate data button works", {
    remDr$open(silent = T)
    remDr$navigate(url = appURL)
    remDr$setImplicitWaitTimeout(milliseconds = 5000)
    # Wait for the app to load
    appTitle <- wait_for_title(remDr, 10, "gfpopgui")
    if (appTitle != "gfpopgui") {
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

    result <- if (result1 & result2) "true" else "false"
    submit_job_info(remDr, buildName,
      name = "the generate data button works",
      result = result
    )
    remDr$close()
  })

  test_that("Can change a penalty value", {
    remDr$open(silent = T)
    remDr$setImplicitWaitTimeout(20000)
    remDr$navigate(url = appURL)
    # Wait for the app to load
    appTitle <- wait_for_title(remDr, 10, "gfpopgui")
    if (appTitle != "gfpopgui") {
      warning("Title is not 'gfpopgui'. Did the application load successfully?")
    }

    # Initalize array of results
    results <- c()

    # Generate some data and move to the analysis tab
    remDr$findElement("id", "home_ui_1-genData")$clickElement()
    remDr$findElement("link", "Analysis")$clickElement()

    # Ensure that the penalty starts at 15
    webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_2']/tbody/tr[2]/td[6]")


    webElem_attr <- webElem$getElementAttribute("innerHTML")[[1]]
    result <- webElem_attr == "15"
    results <- c(results, result)
    expect_equal(webElem_attr, "15")

    # Change the penalty to 150
    webElem <- remDr$findElement("id", "analysis_ui_1-pen")
    webElem$sendKeysToElement(list(key = "delete"))
    webElem$sendKeysToElement(list(key = "delete"))
    webElem$sendKeysToElement(list("150"))
    remDr$findElement("id", "analysis_ui_1-updateGraph")$clickElement()

    # Now ensure the penalty is 150
    webElem <- remDr$findElement("xpath", "//table[@id='DataTables_Table_3']/tbody/tr[2]/td[6]")
    webElem_attr <- webElem$getElementAttribute("innerHTML")[[1]]
    result <- webElem_attr == "150"
    results <- c(results, result)
    expect_equal(webElem_attr, "150")

    result <- if (all(results)) "true" else "false"
    submit_job_info(remDr, buildName,
      name = "Can change a penalty value",
      result = result
    )
    remDr$close()
  })
  
  remDr$close()
  }
}
