.onLoad <- function(libname, pkgname) {
  options("DataPackageR_interact" = interactive())
  options("DataPackageR_packagebuilding" = FALSE)
}
