.onLoad <- function(libname, pkgname) {
  # keeping this first option hardcoded on load for now
  options("DataPackageR_packagebuilding" = FALSE)
  # respect previous user setting for options if set
  op <- options()
  op.DataPackageR <- list(
    DataPackageR_interact = interactive(),
    DataPackageR_verbose = TRUE
  )
  toset <- !(names(op.DataPackageR) %in% names(op))
  if (any(toset)) options(op.DataPackageR[toset])
  invisible()
}
