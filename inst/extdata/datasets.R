pkgName <- roxygen2:::read.description("../DESCRIPTION")$Package

# ------------------------------------------------------------
# Source additional R scripts to preprocess assay data

library(rmarkdown)
render('myPreprocessingCode.Rmd', envir=topenv(), output_dir='../inst/extdata/Logfiles', intermediates_dir='../inst/extdata/Logfiles', clean=FALSE)

# for a systematically-named sequence of scripts, one could do something like this:
# for(fn in list.files(path="./", pattern="^preprocess_.*\\.R$")){
#   sys.source(fn, envir=topenv())
# }


# ------------------------------------------------------------
# Define data objects to keep in the package
# (defining here because the list is useful when building roxygen documentation)
objectsToKeep <- c('myFile1', 'myFile2', 'etc.') # if it's a collection of unsystematically-named objects
# objectsToKeep <- ls(pattern=pkgName) # if you can define a rule that describes the naming of objects to be available in the package

# ------------------------------------------------------------
# Auto build roxygen documentation
# On first build, we generate boilerplate roxygen documentation using preprocessData:::.autoDoc()
# User then manually edits the output file edit_and_rename_to_'documentation.R'.R and renames it to documentation.R.
# The documentation.R file is then used for all subsequent builds.
if(file.exists("documentation.R")){
  sys.source('documentation.R', envir=topenv())
} else {
  preprocessData:::.autoDoc(pkgName, objectsToKeep, topenv())
}

# keep only objects labeled for retention
keepDataObjects(objectsToKeep)  
