.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "gfpopgui"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}