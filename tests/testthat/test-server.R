library(testthat)

mod_home_server_test <- function(id) {
  moduleServer(id, mod_home_server)
}

shiny::testServer(mod_home_server_test, {
  expect_equal(gfpop_data$main_data, NULL) 
})
