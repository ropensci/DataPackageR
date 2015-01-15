.installScript<-function () 
{
  onWindows <- (.Platform$OS.type == "windows")
  files <- "preprocessData"
  if (onWindows) 
    files <- "preprocessData.bat"
  srcDir <- system.file("script", package = "preprocessData")
  srcFile <- file.path(srcDir, files)
  destDir <- file.path(Sys.getenv("R_HOME"), "bin")
  destFile <- file.path(destDir, files)
  alreadyExists <- all(file.exists(destFile))
  if ((!alreadyExists)) {
    res <- FALSE
    suppressWarnings(tryCatch({
      res <- all(file.copy(srcFile, destDir, overwrite = TRUE))
    }, error = function(e) res = -1))
    destFiles <- file.path(destDir, basename(srcFile))
    res <- all(file.exists(destFiles))
    if (interactive()) 
      func <- packageStartupMessage
    else func <- message
    if (is.null(res) || !res || res == -1) {
      script <- "preprocessData"
      if (onWindows) 
        script <- "preprocessData"
      msg <- strwrap(paste("Failed to copy the", paste0("script/", 
                                                        script), "script to", paste0(file.path(Sys.getenv("R_HOME"), 
                                                                                               "bin"), "."), "If you want to be able to run 'R CMD BiocCheck' you'll", 
                           "need to copy it yourself to a directory on your PATH,", 
                           "making sure it is executable.", "See the BiocCheck vignette for more information."))
      for (i in 1:length(msg)) func(msg[i])
    }
    else {
      func("preprocessData script installed.")
    }
  }
}

.isScriptInstalled<-function () 
{
  if (nchar(Sys.which("preprocessData"))) 
    return(TRUE)
  onWindows <- (.Platform$OS.type == "windows")
  if (onWindows) 
    file <- "preprocessData.bat"
  else file <- "preprocessData"
  path <- file.path(Sys.getenv("R_HOME"), "bin")
  all(file.exists(file.path(path, file)))
}

.onLoad <- function (libname, pkgname) 
{
  if (.isScriptInstalled()) 
    return()
  .installScript()
}

