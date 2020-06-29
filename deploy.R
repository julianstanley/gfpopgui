library(rsconnect)
devtools::install_github("julianstanley/gfpopgui")

setAccountInfo(
  name = Sys.getenv("shinyapps_name"),
  token = Sys.getenv("shinyapps_token"),
  secret = Sys.getenv("shinyapps_secret"))

rsconnect::deployApp(forceUpdate = TRUE)