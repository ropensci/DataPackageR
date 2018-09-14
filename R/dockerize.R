

.getSessionInfo <- function(){
  sessionInfo <- sessionInfo()
  sessionInfo
}

.saveSessionInfo <- function(){
  sessionInfo <- .getSessionInfo()
  save(sessionInfo, file = file.path(project_extdata_path(),"sessionInfo.RData"))
}

#' @importFrom containerit Cmd dockerfile
.buildCommand <- function() {
  cmd <- containerit::Cmd(exec = "/bin/bash", params = c("-c", "Rscript -e 'DataPackageR::package_build()';cp ../*.tar.gz /host"))
  cmd
}

#'@importFrom testthat capture_warnings
.buildDockerFile <- function(exclude = NULL){
  cmd <- .buildCommand()
  .saveSessionInfo()
    warns <- testthat::capture_warnings(df <- dockerfile(from = project_extdata_path("sessionInfo.RData"), copy = "script_dir", cmd = cmd))
    w <- rep(FALSE,length(warns))  
    if (!is.null(exclude)) {
      for (e in exclude) {
        w <- w | grepl(e,warns)
      }
    }
    warns <- warns[!w]
    if ( length(warns) != 0) {
      cat(crayon::inverse("These packages should be installed from GitHub or CRAN\n"))
      .bullet(crayon::inverse(warns), bullet = "\u274c")
    }
    df@instructions[[1]]@commands[2] <- paste0(df@instructions[[1]]@commands[2]," \\\n\tzlib1g-dev")
    return(df)
}

.writeDockerFile <- function(exclude = NULL) {
  df <- try(.buildDockerFile(exclude = exclude))
  if (!inherits(df,"try-error")) {
    containerit::write(df,file = file.path(project_path(),"Dockerfile"))
  }
  return(df)
}

