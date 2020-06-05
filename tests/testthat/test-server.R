library(testthat)

mod_home_server_test <- function(id) {
  moduleServer(id, mod_home_server)
}

shiny::testServer(mod_home_server_test, {
  expect_equal(get_main_data(), NULL) 
})
