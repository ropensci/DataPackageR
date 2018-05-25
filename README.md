
<!-- README.md is generated from README.Rmd. Please edit that file -->
DataPackageR
============

Process raw data into packaged, analysis-ready data sets, reproducibly.

NEWS
-------
Lots of changes in the yaml branch.
    
    - package uses config.yml rather than datasets.R
    - primary mechanism for setup is to pass scripts and named R objects in via `datapackage.skeleton()`. These populate the yaml config.
    - documenation.R is created automatically on first build via autodoc.
    - Package can be built successfully on first run of buildDataSetPackage thanks to the above.
    - Rmarkdown based scripts are the primary mechanism for processing. R files will still work.
    - Goal is to eliminate the need to write more R code for configuration.
    - yaml configuration is in `packageroot/data-raw` but might be moved to the package root
    - No longer using the `here()` function as it's not suitable for non-interactive use
    - Leverage rprojroot and proper logging via `futile.logger` package, do proper path normalization.
    - Added some basic unit tests - more to come

# TODO
-------
     - update the vignettes to demonstrate new features
     - Fix the mechanism for partial building as it's broken via the latest yaml config changes.

Overview
--------

A data set may consist of multiple sources of raw data. These need to be cleaned, standardized, QCd, and any number of other manipulations applied before they are ready for analysis.

This package is designed to simplify and centralize tasks associated with tidying a variety of data sets associated with a single project.

It provides a mechanism to collect, organize and version the data munging scripts used to process incoming data into analytical data sets. The package runs these scripts to perform the data munging writes out analytical data sets, which are combined with documentation, and built into a new R package. The package and the included data are versioned. If updated data arrive, the package can be rebuilt, and the data version incremented to reflect the changes. If the data munging code is in the form of Rmarkdown documents, these are processed automatically into package vignettes that are included in the final pacakge.

No other restrictions are placed on the data munging code.

Usage
-----

Set up a new data package. Assume we have data munging code in `MungeDataset1.Rmd`, and `MungeDatast2.Rmd`, and each of these produce R objects `dataset1` and `dataset2`.

``` r
library(DataPackageR)
setwd("/tmp")
DataPackageR::datapackage.skeleton("MyNewStudy",force=TRUE,code_files = c("/tmp/MungeDataset1.Rmd","/tmp/MungeDataset2.Rmd"),r_object_names = c("dataset1","dataset2"))
```

    Creating directories ...
    Creating DESCRIPTION ...
    Creating NAMESPACE ...
    Creating Read-and-delete-me ...
    Saving functions and data ...
    Making help files ...
    Done.
    Further steps are described in './MyNewStudy/Read-and-delete-me'.
    Adding DataVersion string to DESCRIPTION
    Creating data and data-raw directories

The above code creates a directory "MyNewStudy" with the skeleton of a data package.

The `DESCRIPTION` file should be filled out to describe your package. A new `DataVersion` string now appears in that file. The revision is automatically incremented if the package data changes.

`Read-and-delete-me` has some helpful instructions on how to proceed.

The `data-raw` directory is where the data cleaning code (`Rmd`) files reside. The contents of this directory are:

    MyNewStudy/data-raw
    └── datasets.R
    └── MungeDataset1.Rmd
    └── MungeDataset2.Rmd

`datasets.R` can be edited as necessary (see below). This "master" file sources your data munging scripts. Data munging scripts can read data from anywhere, but it is good practice to have your "raw" data live under `/inst/extdata`. It should be copied into that path and the data munging scripts edited appropriately.

Here are the contents on `datasets.R`:

    pkgName <- roxygen2:::read.description("../DESCRIPTION")$Package

    # ------------------------------------------------------------
    # Source additional R scripts to preprocess assay data

    library(rmarkdown)
    render('MungeDataset1.Rmd', envir=topenv(), output_dir='../inst/extdata/Logfiles', clean=FALSE)
    render('MungeDataset2.Rmd', envir=topenv(), output_dir='../inst/extdata/Logfiles', clean=FALSE)

    # for a systematically-named sequence of scripts, one could do something like this:
    # for(fn in list.files(path="./", pattern="^preprocess_.*\\.Rmd$")){
    #   render(fn, envir=topenv(),output_dir="../inst/extdata/Logfiles",clean=FALSE)
    # }
    # Or a full path to each Rmd file can be passed to datapacakge.skeleton via code_files.


    # ------------------------------------------------------------
    # Define data objects to keep in the package
    # (defining here because the list is useful when building roxygen documentation)
    objectsToKeep <- c('dataset1', 'dataset2', 'etc.') # if it's a collection of unsystematically-named objects
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
    keepDataObjects(objectsToKeep)  

We look at this piece by piece.

### Data processing scripts

First, we load the rmarkdown package and then render the user-provided data processing code `MungeDataset1.Rmd`, and `MungeDataset2.Rmd`.

-   This should (obviously) be an Rmarkdown file, that combines text and R code.
-   It should contain a YAML preamble with the minimum information needed to process it into an html report.
-   When run, it should perform the processing of your data sets(s) into an R object named in `r_object_names` of `datapackage.skeleton`.

The product of this particular script will be an html document that serves as a log of how the data were processed.

-   The html report will be included as a `vignette` in the final package.

The most important product of processing script is one or more R objects.

-   The call to `keepDataObjects()` tells the build process which objects should be retained and stored as part of the data package.
-   In this case, our scripts should produce two objects `dataset1` and `dataset2`.
-   `keepDataObjects('dataset1','dataset2')` tells the build process the name of the object to store in the package.
-   All this is taken care of via arguments to `datapackage.skeleton`.

-   You do not need to save these objects to `data`. The build process will handle this for you.
-   The objects need to exist in memory when the processing script is finished running.
-   The build process will match these names to objects in memory and to existing documentation (see below).
-   If everything is in order, they will be included in the built package.

### Object Documentation

There is a call to `.autoDoc`, which generates documentation for the package and the objects on the first run of the build.

It produces a file that the user needs to rename and edit by hand.

The contents of this file are roxygen blocks that are parsed into object and package documentation.

-   It is good practice to
    -   Document all the columns of tables in your data set.
    -   Include the source of the data (i.e. where the data came from).

### Build your package.

Once your scripts are in place and the data objects are documented, you build the package.

To run the build process:

``` r{}
# Within the package directory
DataPackageR:::buildDataSetPackage(".") #note for a first build this needs to be run twice and the 
#documentation edited.
```

If there are errors, the script will notify you of any problems.

-   Correct any errors and rerun the build process.

If everything goes smoothly, you will have a new package built in the parent directory.

This can be distributed, installed using `R CMD INSTALL`, and data sets loaded using R's standard `data()` call. Vignettes can be interrogated via `vignette(package="mypackage")`

### Data versioning

The DataPackageR package calculates an md5 checksum of each data object it stores, and keeps track of them in a file called `DATADIGEST`.

-   Each time the package is rebuilt, the md5 sums of the new data objects are compared against the DATADIGEST.
-   If they don't match, the build process checks that the `DataVersion` string has been incremented in the `DESCRIPTION` file.
-   If it has not the build process will exit and produce an error message.

### Benefits

Your downstream data analysis can depend on a specific version of your data package (for example by tesing the `packageVersion()` string);

``` r{}
if(packageVersion("MyNewStudy") != "1.0.0")
  stop("The expected version of MyNewStudy is 1.0.0, but ",packageVersion("MyNewStudy")," is installed! Analysis results may differ!")
```

The DataPackageR packge also provides `datasetVersion()` to extract the data set version information.

You should also place the data package source directory under `git` version control. This allows you to version control your data processing code.
