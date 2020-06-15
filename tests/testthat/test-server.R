library(testthat)

# app_server.R -----------------------------------------------------------------
app_server_test <- function(id) {
  moduleServer(id, app_server)
}

shiny::testServer(app_server_test, {
  expect_equal(typeof(gfpop_data), "list") 
  expect_equal(names(gfpop_data), c("graphdata_visNetwork", "graphdata"))
})


# mod_home_server.R ------------------------------------------------------------
mod_home_server_test <- function(id) {
  moduleServer(id, mod_home_server)
}

# Initial values
shiny::testServer(mod_home_server_test, {
  expect_equal(names(gfpop_data), c("graphdata_visNetwork", "graphdata"))
  expect_equal(gfpop_data$main_data, NULL) 
  print(output$main_datatable)
})

# After setting main data
shiny::testServer(mod_home_server_test, {
  print(output$main_datatable)
  session$setInputs(ndata = 100)
  session$setInputs(sigma = 1)
  generate_data()
  expect_equal(names(gfpop_data), c("graphdata_visNetwork", 
                                    "graphdata", "main_data"))
  print(gfpop_data$main_data)
  print(length(output$main_datatable))
})

# mod_analysis_server.R---------------------------------------------------------
mod_analysis_server_test <- function(id, gfpop_data) {
  moduleServer(id, function(input, output, session) {
    mod_analysis_server(input, output, session, gfpop_data)
  })
}

shiny::testServer(mod_analysis_server_test, {
  print(gfpop_data)
}, args = list(gfpop_data = 1))
