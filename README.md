# preprocessData
An R package to allow `R CMD preprocessData packagename` to run some preprocessing (i.e. data tidying) on raw data by running code in `packagename/data-raw` and generate standardied data sets or objects in `packagename/data`. The package will verify whether data has chanaged based on a digest of the data objects, and enables data versioning using the DataVersion string in the DESCRIPTION file. Documentation, if available as roxygen2 code in the .R files, will be extracted and placed in the "/R" directory.

## Sourcing other R files from `datasets.R`
Your code must source other R files via `sys.source("myotherRfile.R",env=topenv())`, otherwise bad things can happen.

## Origins
This brings together ideas by Robert Gentleman,  Yihui Xie, Hadley Wickham, and many others. Most of these concepts have been around as long as R and the idea of *literate programming*. Effort and time invested by the people above in building fantastic tools has made generating reproducible research easier than ever before. 

## Motivation
Our group needs to standardize a lot of data across many projects and distribute it to many people. The data can change over time. We need to ensure they all work with the same version of the data. 

In general, if you work in R, you'll have raw data that you'd like to clean up, preprocess, or otherwise standardize in order to use it in your own projects or share it with others. You could do this with a series of shell scripts, and R script, a `knitr` markdown document, or any other number of ways to generate some `.csv` files or R `data.frames` or `data.tables` or other complex R objects that represent your data. You might then save these into the `/data` directory of an R data package, build and distribute. 

If the data change and you're using the R package system, you get versioning for free, as long as you're aware the data have change and you remember to bump the version number. So, just process your data again and update the contents of `/data` and you're good to go.

R packages process *markdown* or *Sweave* documents in the `/vignettes` directory during the build process, but that doesn't quite fit our needs. The vignettes may rely on some of the data they are supposed to generate and the vignette build process needs to take no more than a certain amount of time in order for packages to meet CRAN and Bioconductor guidelines. Finally, do you really need to generate a 50-page document detailing how the data was cleaned each time the package is built? Ideally, we'd like to separete out the data preprocessing from the build stage, but still have it be an integral part of the R package development process. 

## How it works

That's where `preprocessData` comes in. It enables the `R CMD preprocessData` command. Based upon `BiocCheck` developed by the BioConductor team, it installs a script (requires admin rights) into `$R_HOME/bin` that enables `R CMD preprocessData packagename` which does the following:

- Requires that you have a `DataVersion: x.y.z` string in the package `DESCRIPTION` file.
- Looks for `datasets.R`  under the directory `packagename/data-raw` and sources it in its own environment. `datasets.R` can source other R files (must be done via `sys.source("myotherRfile.R",env=topenv())` )
- Computes a digest of the R objects that are created.
- Compares this against the contents of `DATADIGEST` if it's available.
- When the digests don't match, the data has changed, and the package requires that you update the `DataVersion` string.
- When all is well, it writes a new `DATADIGEST` to the package source directory and writes the data objects to the `/data` directory.
- Extracts `roxygen2` documentation from the `R` files that matches the R objects and places it in `/R` so it can be processed by `roxygenize`. 

The `datasets.R` file and any files it sources can read raw data from any source which may or may not be in `/inst/extdata`. They should tidy the data and standardize it as necessary.

**To reiterate**
- The `preprocessData` package takes care of saving the standardized data to the `/data` directory as `packagename.rda` so that it can be loaded with `data(packagename)`, provided it 
    1. Has not changed based on an md5 digest of the objects.
    2. If it has changed, the DataVersion string has been incremented in the DESCRIPTION file.

- The package extracts `roxygen2` documentation frmo `/data-raw`,  and places it in `/R/packagename.R`   
- `R CMD build packagename` generates `packagename_x.y.z.tar.gz` with documented and standardized data available via the usual `data()` mechanism. 
- Unless `data-raw` is placed in `.Rbuildignore` the code to generate the data sets is distributed with the package.

**Shortcuts and convenience functions**
The package provides an R function to process, document, and build a package with a single call. 
`buildDataSetPackage("mypackage")` is equivalent to:   

`$ R CMD preprocessData mypackage`  
`$ Rscript -e 'roxygen2::roxygenize("mypackage")'`  
`$ R CMD build mypackage`  

`dataVersion("packagename")` returns the DataVersion of the specified package.
`datapackage.skeleton()` invokes `package.skeleton()` and populates the directory with additional information required by `preprocessData`. 

## Benefits

Using the pacakge build system we get:
- Versioning of data and package versioning.
- Relatively standardized build, install, and distribution mechanism
- Enforced dependencies between packages ( meaning we can have packages that wrap up an analysis of a project and depend on a specific version of a data package).
- Standard mechanism to run unit tests to verify the integrity of the data (these go into `/tests` and are run automatically during build). 
- All of this improves reproducibility and takes some of the worry out of working on multiple projects simultaneously.

## Ongoing work

While tools like `packrat` and others aim to do the above by fixing the version of all packages used for a project into a local library bundle that can be distributed, this can be a bit of overkill, particularly if all consumers of a data set are using the same environment. Recording the `sessionInfo()` may be sufficient in many cases (since we are mostly only concerned with data tidying). Some new tools from Robert Gentleman's group could enable users to store and verify the versions of packages used for preprocessing data sets between package builds. This remains to be explored further.


## Known limitations.

- The current code parses a single R file named `datasets.R`. This file can source other .R files under `/data-raw`. This must be done via `sys.source("myotherRfile.R",env=topenv())`.
- Documentation is extracted from all .R files under `/data-raw`, and collated and matched to the data objects generated by the .R files. No checking is done for name collisions.
- A file named `packagename.R` is generated under `/R` containing the roxygen2 documentation for the data sets. This overwrites any .R file with the same name already present under the `/R` directory without warning.