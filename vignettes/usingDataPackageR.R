## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval=TRUE
)

## ------------------------------------------------------------------------
library(data.tree)
library(DataPackageR)
processing_code = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
print(processing_code)
setwd("/tmp")
DataPackageR::datapackage.skeleton("Test", 
                                   force=TRUE, 
                                   code_files = processing_code, 
                                   r_object_names = "cars_over_20") # cars_over_20 is an R object 
                                                                    # created in the Rmd file.

## ----dirstructure,echo=FALSE---------------------------------------------
df = data.frame(pathString=file.path("Test",(list.files("/private/tmp/Test",recursive=TRUE))))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
library(yaml)
setwd("/tmp")
print(as.Node(yaml.load_file("Test/datapackager.yml")),"files","objects")

## ------------------------------------------------------------------------
# Within the package directory
setwd("/tmp")
DataPackageR:::buildDataSetPackage("Test") 

## ---- results='asis',echo=FALSE------------------------------------------
library(yaml)
setwd("/tmp")
df = data.frame(pathString=file.path("Test",(list.files("Test",recursive=TRUE))))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
setwd("/tmp")
cat(readLines("Test/DATADIGEST"),sep="\n")

## ----echo=FALSE----------------------------------------------------------
setwd("/tmp")
cat(readLines("Test/DESCRIPTION"),sep="\n")

