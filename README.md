
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataPackageR

DataPackageR is used to reproducibly process raw data into packaged,
analysis-ready data sets.

[![Build
Status](https://travis-ci.org/RGLab/DataPackageR.svg?branch=master)](https://travis-ci.org/RGLab/DataPackageR)
[![Coverage
status](https://codecov.io/gh/RGLab/DataPackageR/branch/master/graph/badge.svg)](https://codecov.io/github/RGLab/DataPackageR?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/RGLab/DataPackageR?branch=master&svg=true)](https://ci.appveyor.com/project/RGLab/DataPackageR)
[![DOI](https://zenodo.org/badge/29267435.svg)](https://doi.org/10.5281/zenodo.1292095)

  - [yaml configuration guide](YAML_CONFIG.md)

## What problems does DataPackageR tackle?

You have diverse raw data sets that you need to preprocess and tidy in
order to:

  - Perform data analysis
  - Write a report
  - Publish a paper
  - Share data with colleagues and collaborators
  - Save time in the future when you return to this project but have
    forgotten all about what you did.

### Why package data sets?

  - **Reproducibility.**
    
    As described [elsewhere](https://github.com/ropensci/rrrpkg),
    packaging your data promotes reproducibility. R’s packaging
    infrastructure promotes unit testing, documentation, a reproducible
    build system, and has many other benefits. Coopting it for packaging
    data sets is a natural fit.

  - **Collaboration.**
    
    A data set packaged in R is easy to distribute and share amongst
    collaborators, and is easy to install and use. All the hard work
    you’ve put into documenting and standardizing the tidy data set
    comes right along with the data package.

  - **Documentation.**
    
    R’s package system allows us to document data objects. What’s more,
    the `roxygen2` package makes this very easy to do with [markup
    tags](http://r-pkgs.had.co.nz/data.html). That documentation is the
    equivalent of a data dictionary and can be extremely valuable when
    returning to a project after a period of time.

  - **Convenience.**
    
    Data pre-processing can be time consuming, depending on the data
    type and raw data sets may be too large to share conveniently in a
    packaged format. Packaging and sharing the small, tidied data saves
    the users computing time and time spent waiting for downloads.

## Challenges.

  - **Package size limits.**
    
    R packages have a 5MB size limit, at least on CRAN. BioCondctor has
    explicit [data
    package](https://www.bioconductor.org/developers/package-guidelines/#package-types)
    types that can be larger and use git LFS for very large files.
    
    Sharing large volumes of raw data in an R package format is still
    not ideal, and there are public biological data repositories better
    suited for raw data: e.g., [GEO](https://www.ncbi.nlm.nih.gov/geo/),
    [SRA](https://www.ncbi.nlm.nih.gov/sra),
    [ImmPort](http://www.immport.org/immport-open/public/home/home),
    [ImmuneSpace](https://immunespace.org/),
    [FlowRepository](https://flowrepository.org/).
    
    Tools like [datastorr](https://github.com/ropenscilabs/datastorr)
    can help with this and we hope to integrate the into DataPackageR in
    the future.

  - **Manual effort**
    
    There is still a substantial manual effort to set up the correct
    directory structures for an R data package. This can dissuade many
    individuals, particularly new users who have never built an R
    package, from going this route.

  - **Scale**
    
    Seting up and building R data packages by hand is a workable
    solution for a small project or a small number of projects, but when
    dealing with many projects each involving many data sets, tools are
    needed to help automate the process.

## DataPackageR

DataPakcageR provides a number of benefits when packaging your data.

  - It aims to automate away much of the tedium of packaging data sets
    without getting too much in the way, and keeps your processing
    workflow reproducible.

  - It sets up the necessary package structure and files for a data
    package.

  - It allows you to keep the large, raw data and only ship the packaged
    tidy data, saving space and time consumers of your data set need to
    spend downloading and re-processing it.

  - It maintains a reproducible record (vignettes) of the data
    processing along with the package. Consumers of the data package can
    verify how the processing was done, increasing confidence in your
    data.

  - It automates construction of the documenation and maintains a data
    set version and an md5 fingerprint of each data object in the
    package. If the data changes and the package is rebuilt, the data
    version is automatically updated.

## Similar work

There are a number of tools out there that address similar and
complementary problems:

  - **datastorr** [github
    repo](https://github.com/ropenscilabs/datastorr)
    
    Simple data retrieval and versioning using GitHub to store data.
    
      - Caches downloads and uses github releases to version data.
      - Deal consistently with translating the file stored online into a
        loaded data object
      - Access multiple versions of the data at once
    
    `datastorrr` could be used with DataPackageR to store / access
    remote raw data sets, remotely store / acess tidied data that are
    too large to fit in the package itself.

  - **fst** [github repo](https://github.com/fstpackage/fst)
    
    `fst` provides lightning fast serialization of data frames.

  - **The modern data package**
    [pdf](https://github.com/noamross/2018-04-18-rstats-nyc/blob/master/Noam_Ross_ModernDataPkg_rstatsnyc_2018-04-20.pdf)
    
    A presenataion from @noamross touching on modern tools for open
    science and reproducibility. Discusses `datastorr` and `fst` as well
    as standardized metadata and documentation.

  - **rrrpkg** [github repo](https://github.com/ropensci/rrrpkg)
    
    A doucment from ropensci describing using an R package as a research
    compendium. Based on ideas originally introduced by Robert Gentleman
    and Duncan Temple Lang (Gentleman and Lang (2004))

  - **template** [github repo](https://github.com/ropensci/rrrpkg)
    
    An R package template for data packages.

See the [publication](#publication) for further discussion.

## Installation

You can install the latest version of DataPackageR from
[github](https://www.github.com/RGLab/DataPackageR) with:

``` r
library(devtools)
devtools::install_github("RGLab/DataPackageR")
```

## Example

``` r
library(DataPackageR)

# Let's reproducibly package up
# the cars in the mtcars dataset
# with speed > 20.
# Our dataset will be called cars_over_20.

# Get the code file that turns the raw data
# to our packaged and processed analysis-ready dataset.
processing_code <- system.file(
  "extdata", "tests", "subsetCars.Rmd", package = "DataPackageR"
)

# Create the package framework.
DataPackageR::datapackage_skeleton(
  "mtcars20", force = TRUE, code_files = processing_code, r_object_names = "cars_over_20", path = tempdir()) 
#> Creating directories ...
#> Creating DESCRIPTION ...
#> Creating NAMESPACE ...
#> Creating Read-and-delete-me ...
#> Saving functions and data ...
#> Making help files ...
#> Done.
#> Further steps are described in '/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T//Rtmpwy0gLc/mtcars20/Read-and-delete-me'.
#> Adding DataVersion string to DESCRIPTION
#> Creating data and data-raw directories
#> configuring yaml file

# Run the preprocessing code to build cars_over_20
# and reproducibly enclose it in a package.
DataPackageR:::package_build(file.path(tempdir(),"mtcars20"))
#> 
#> 
#> processing file: subsetCars.Rmd
#> output file: subsetCars.knit.md
#> 
#> Output created: /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmpwy0gLc/mtcars20/inst/extdata/Logfiles/subsetCars.html
#> First time using roxygen2. Upgrading automatically...
#> Updating roxygen version in /private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmpwy0gLc/mtcars20/DESCRIPTION
#> '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
#>   --no-environ --no-save --no-restore --quiet CMD build  \
#>   '/private/var/folders/jh/x0h3v3pd4dd497g3gtzsm8500000gn/T/Rtmpwy0gLc/mtcars20'  \
#>   --no-resave-data --no-manual --no-build-vignettes
#> 

# Let's use the package we just created.
install.packages(file.path(tempdir(),"mtcars20_1.0.tar.gz"), type = "source", repos = NULL)
library(mtcars20)
data("cars_over_20") # load the data
cars_over_20  # Now we can use it.
?cars_over_20 # See the documentation you wrote in data-raw/documentation.R.

# We have our dataset!
# Since we preprocessed it,
# it is clean and under the 5 MB limit for data in packages.
cars_over_20

# We can easily check the version of the data
DataPackageR::data_version("mtcars20")

# You can use an assert to check the data version in  reports and
# analyses that use the packaged data.
assert_data_version(data_package_name = "mtcars20",
                    version_string = "0.1.0",
                    acceptable = "equal")
```

### Reading external data

In an Rmd file, external data (stored in `inst/extdata` at the data
package source, or eslewhere) can be located relative to:

``` r
# This returns the datapackage source 
# root directory. 
DataPackageR::project_path()

# This returns the datapackage  
# inst/extdata directory. 
DataPackageR::project_extdata_path()

# This returns the path to the datapackage  
# data directory. 
DataPackageR::project_data_path()
```

## Preprint and publication. <a id = "publication"></a>

The publication describing the package, Finak et al. (2018), is now
available at [Gates Open
Research](https://gatesopenresearch.org/articles/2-31/v1) .

The preprint is on [biorxiv](https://doi.org/10.1101/342907).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

# References

<div id="refs" class="references">

<div id="ref-Finak2018-tu">

Finak, Greg, Bryan Mayer, William Fulp, Paul Obrecht, Alicia Sato, Eva
Chung, Drienna Holman, and Raphael Gottardo. 2018. “DataPackageR:
Reproducible Data Preprocessing, Standardization and Sharing Using
R/Bioconductor for Collaborative Data Analysis.” *bioRxiv*.

</div>

<div id="ref-Gentleman2004-oj">

Gentleman, Robert, and Duncan Temple Lang. 2004. “Statistical Analyses
and Reproducible Research.” *Bioconductor Project Working Papers*,
Bioconductor project working papers,. bepress.

</div>

</div>
