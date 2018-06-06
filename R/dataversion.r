

#' Get the DataVersion for a package
#'
#' Retreives the DataVersion of a package if available
#' @param pkg \code{character} the package name
#' @param lib.loc \code{character} path to library location.
#' @seealso \code{\link[utils]{packageVersion}}
#' @import utils
#' @import futile.logger
#' @export
dataVersion <- function(pkg, lib.loc = NULL) {
  res <- suppressWarnings(
    packageDescription(pkg,
                       lib.loc = lib.loc,
                       fields = "DataVersion"))
  if (!is.na(res))
    package_version(res)
  else  
    stop(gettextf(paste0("package %s not found",
                  "or has no DataVersion string"),
                  sQuote(pkg)),
         domain = NA)
}
.increment_data_version <-
  function(pkg_description, new_data_digest, which = "patch") {
    if (!which %in% c("major", "minor", "patch")) {
      stop(
        paste0("version component to increment",
               "is misspecified in ",
               ".increment_data_version, ",
               "package DataPackageR")
      )
    }
    verstring <- strsplit(pkg_description[["DataVersion"]], "\\.")[[1]]
    names(verstring) <- c("major", "minor", "patch")
    verstring[which] <-
      as.character(as.numeric(verstring[which]) + 1)
    verstring <- paste(verstring, collapse = ".")
    pkg_description[["DataVersion"]] <- verstring
    new_data_digest[["DataVersion"]] <- verstring
    list(pkg_description = pkg_description, new_data_digest = new_data_digest)
  }
