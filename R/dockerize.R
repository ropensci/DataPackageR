

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

.buildDockerFile <- function(){
  cmd <- .buildCommand()
  .saveSessionInfo()
  df <- containerit::dockerfile(from = project_extdata_path("sessionInfo.RData"), copy = "script_dir", cmd = cmd)
  df@instructions[[1]]@commands[2] <- paste0(df@instructions[[1]]@commands[2]," \\\n\tzlib1g-dev")
  df
}

.writeDockerFile <- function() {
  df <- .buildDockerFile()
  containerit::write(df,file = file.path(project_path(),"Dockerfile"))
  return(df)
}

