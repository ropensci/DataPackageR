# -----------------------------------------------------------
# autoDoc.R
# an R script to automate the creation of a basic roxygen template
# -----------------------------------------------------------


# function rc() prepends "#'" to a string or character vector
rc <- function(strVec){paste("#'", strVec)}


# create list of "see-also"s, with name of package as the first element.
# uses objectsToKeep from datasets.R as a list of every data set worth documenting.
# links is the list, linksRox is the same list formatted as Roxygen \link{}s
links <- c(pkgName, objectsToKeep)
linksRox <- paste0("\\link{", links, "}")


# create default file to be edited and renamed manually by user, who then rebuilds package
tempfileName <- "./edit_and_rename_to_'documentation.R'.R"
if(file.exists(tempfileName)){file.remove(tempfileName)}

con <- file(tempfileName, open = "w")

# create Roxygen documentation for data package
writeLines(
  c(rc(
    c(pkgName,
      paste0("A data package for ", pkgName, "."),
             "@docType package",
      paste0("@aliases ", pkgName, "-package"),
             "@title Package Title",
      paste0("@name ", pkgName), 
             "@description A description of the data package",
      paste0("@details Use \\code{data(package='", pkgName, "')$results[, 3]} to see a list of"),
             "available data sets in this data package.",
             "@seealso",
      linksRox[2:length(links)])),
      "NULL\n\n"), con)

# Cycle through the rest of the files listed in 'links' and create Roxygen documentation for each one
for(ds in links[2:length(links)]){
  type <- class(get(ds))[1]
  writeLines(
    rc(
      c(       "Detailed description of the data",
         paste("@name", ds),
               "@docType data",
               "@title Descriptive data title",
        paste0("@format a \\code{", type, "} containing the following fields:"),
               "\\describe{")), con)

  # set up documentation template for each field, using \item{varname}{} with a blank description to fill in
  for(var in names(get(ds))){
    writeLines(rc(paste0("\\item{", var, "}{}")), con)
  }

  writeLines(
    c(rc(
        c("}",
          "@source The data comes from ___.",
          "@seealso",
          linksRox[which(links != ds)])), # dataset being documented should not list itself in its links
          "NULL\n\n"), con)
}

close(con)
