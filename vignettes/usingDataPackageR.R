## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval=TRUE
)

## ------------------------------------------------------------------------
library(data.tree)
library(DataPackageR)
tmp = normalizePath(tempdir())
processing_code = system.file("extdata","tests","subsetCars.Rmd",package="DataPackageR")
print(processing_code)
setwd(tmp)
DataPackageR::datapackage.skeleton("Test", 
                                   force=TRUE, 
                                   code_files = processing_code, 
                                   r_object_names = "cars_over_20") # cars_over_20 is an R object 
                                                                    # created in the Rmd file.

## ----dirstructure,echo=FALSE---------------------------------------------
df = data.frame(pathString=file.path("Test",(list.files(tmp,recursive=TRUE))))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
library(yaml)
setwd(tmp)
cat(as.yaml(yaml.load_file("Test/datapackager.yml")))

## ------------------------------------------------------------------------
# Within the package directory
setwd(tmp)
DataPackageR:::package_build("Test") 

## ---- echo=FALSE---------------------------------------------------------
library(yaml)
setwd(tmp)
df = data.frame(pathString=file.path("Test",(list.files("Test",recursive=TRUE))))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
setwd(tmp)
cat(readLines("Test/DATADIGEST"),sep="\n")

## ----echo=FALSE----------------------------------------------------------
setwd(tmp)
cat(readLines("Test/DESCRIPTION"),sep="\n")

## ----construct_config----------------------------------------------------
#assume I have file1.Rmd and file2.R located in /data-raw, and these create 'object1' and 'object2' respectively.

config = construct_yml_config(code = c("file1.Rmd","file2.R"), data = c("object1","object2"))
print(config)

## ------------------------------------------------------------------------
path_to_package = tempdir() #pretend this is the root of our package
yml_write(config,path = path_to_package)

## ------------------------------------------------------------------------
config = yml_disable_compile(config,filenames = "file2.R")
print(as.yaml(config))

