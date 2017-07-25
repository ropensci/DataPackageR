pkgName <- roxygen2:::read.description("../DESCRIPTION")$Package

# ------------------------------------------------------------
# Source additional R scripts to preprocess assay data

library(rmarkdown)
render('myPreprocessingCode.Rmd', envir=topenv(), output_dir='../inst/extdata/Logfiles', intermediates_dir='../inst/extdata/Logfiles', clean=FALSE)

# for a systematically-named sequence of scripts, one could do something like this:
# for(fn in list.files(path="./", pattern="^preprocess_.*\\.Rmd$")){
#   render(fn, envir=topenv(),output_dir="../inst/extdata/Logfiles",intermediates_dir = "../inst/extdata/Logfiles",clean=FALSE)
# }
# Or a full path to each Rmd file can be passed to datapacakge.skeleton via code_files.


# ------------------------------------------------------------
# Define data objects to keep in the package
# (defining here because the list is useful when building roxygen documentation)
objectsToKeep <- c('myFile1', 'myFile2', 'etc.') # if it's a collection of unsystematically-named objects
# objectsToKeep <- ls(pattern=pkgName) # if you can define a rule that describes the naming of objects to be available in the package
# Or these can be passed into datapackage.skeleton via the r_object_names parameter

# ------------------------------------------------------------
# Auto build roxygen documentation
# On first build, we generate boilerplate roxygen documentation using DataPackageR:::.autoDoc()
# User then manually edits the output file edit_and_rename_to_'documentation.R'.R and renames it to documentation.R.
# The documentation.R file is then used for all subsequent builds.
if(file.exists("documentation.R")){
  sys.source('documentation.R', envir=topenv())
} else {
  DataPackageR:::.autoDoc(pkgName, objectsToKeep, topenv())
}

# keep only objects labeled for retention
DataPackageR:::keepDataObjects(objectsToKeep)  
