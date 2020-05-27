pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

setAccountInfo(
  name = Sys.getenv("shinyapps_name"),
  token = Sys.getenv("shinyapps_token"),
  secret = Sys.getenv("shinyapps_secret"))

rsconnect::deployApp()