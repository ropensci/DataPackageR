## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  eval = TRUE
)

## ----minimal_example, results='hide'-------------------------------------
library(DataPackageR)

# Let's reproducibly package up
# the cars in the mtcars dataset
# with speed > 20.
# Our dataset will be called cars_over_20.

# Get the code file that turns the raw data
# to our packaged and processed analysis-ready dataset.
processing_code <-
  system.file("extdata", 
              "tests",
              "subsetCars.Rmd",
              package = "DataPackageR")

# Create the package framework.
DataPackageR::datapackage_skeleton(
  "mtcars20",
  force = TRUE,
  code_files = processing_code,
  r_object_names = "cars_over_20",
  path = tempdir()
  ) 

## ----dirstructure,echo=FALSE---------------------------------------------
library(data.tree)
df = data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
as.Node(df)

## ---- echo=FALSE---------------------------------------------------------
cat(yaml::as.yaml(yaml::yaml.load_file(file.path(tempdir(),"mtcars20","datapackager.yml"))))

## ----eval=TRUE-----------------------------------------------------------
# Run the preprocessing code to build cars_over_20
# and reproducibly enclose it in a package.
DataPackageR:::package_build(file.path(tempdir(),"mtcars20"))

## ---- echo=FALSE---------------------------------------------------------
df = data.frame(pathString = file.path(
  "mtcars20",
  list.files(
  file.path(tempdir(), "mtcars20"),
  include.dirs = TRUE,
  recursive = TRUE
  )
  ))
  as.Node(df)

## ------------------------------------------------------------------------
# Let's use the package we just created.
install.packages(file.path(tempdir(),"mtcars20_1.0.tar.gz"), type = "source", repos = NULL)
if(!"package:mtcars20"%in%search())
  attachNamespace('mtcars20') #use library() in your code
data("cars_over_20") # load the data

cars_over_20 # now we can use it.
?cars_over_20 # See the documentation you wrote in data-raw/documentation.R.

vignettes = vignette(package="mtcars20")
vignettes$results

## ------------------------------------------------------------------------
# We can easily check the version of the data
DataPackageR::data_version("mtcars20")

# You can use an assert to check the data version in  reports and
# analyses that use the packaged data.
assert_data_version(data_package_name = "mtcars20",
                    version_string = "0.1.0",
                    acceptable = "equal")  #If this fails, execution stops
                                           #and provides an informative error.

## ----construct_config, echo=1:2------------------------------------------
# assume I have file1.Rmd and file2.R located in /data-raw, 
# and these create 'object1' and 'object2' respectively.

config = construct_yml_config(code = c("file1.Rmd", "file2.R"),
                              data = c("object1", "object2"))
cat(yaml::as.yaml(config))

## ------------------------------------------------------------------------
path_to_package = tempdir() #e.g., if tempdir() was the root of our package.
yml_write(config, path = path_to_package)

## ----echo=1:2------------------------------------------------------------
config = yml_disable_compile(config,filenames = "file2.R")
yml_write(config, path = path_to_package) # write modified yml to the package.
cat(yaml::as.yaml(config))

## ---- echo=FALSE---------------------------------------------------------
cat(readLines(file.path(tempdir(),"mtcars20","DATADIGEST")),sep="\n")

## ----echo=FALSE----------------------------------------------------------
cat(readLines(file.path(tempdir(),"mtcars20","DESCRIPTION")),sep="\n")

