# preprocessData
An R package to allow `R CMD preprocessData packagename` to run some preprocessing (i.e. data tidying) on raw data by running code in `packagename/data-raw` and generate standardied data sets or objects in `packagename/data`. The package will verify whether data has chanaged based on a digest of the data objects, and enables data versioning using the DataVersion string in the DESCRIPTION file. Documentation, if available as roxygen2 code in the .R files, will be extracted and placed in the "/R" directory.

## Sourcing other R files from `datasets.R`
Your code must source other R files via `sys.source("myotherRfile.R",env=topenv())`, otherwise bad things can happen.

## Origins
This brings together some ideas by Robert Gentleman,  Yihui Xie, Hadley Wickham, and many others. Most of these concepts have been around as long as R and the idea of *literate programming*. Effort and time invested by the people above in building fantastic tools has made generating reproducible research easier than ever before. 

## Motivation
Our group needs to standardize a lot of data, across many projects, distribute it, and many people will be working with it. Not all of these data consumers will be R experts. We need to ensure they all work with the same version of the data (which can change over time). 

Say you work in R. Say you have raw data that you'd like to clean up, preprocess, and standardize in order to share it with others or use in your own projects. You could do this with a series of shell scripts, and R script, a `knitr` markdown document, or any other number of ways to generate some `.csv` files or R `data.frames` or `data.tables` or other complex R objects that represent your data. You might then save these into the `/data` directory of your new R package, build and distribute. 

But what if your raw data change? Say you gather or receive more data? Because your data are in an R package, you get versioning for free. So, just process your data again and update the `.rda` files in `/data` and you're good to go.

But where did you put that processing code? Maybe you were clever and it's under version control somewhere. Or maybe you weren't so clever, and it's in some directory, now you need to find it.. but is this the version of the code that generated the data in that package? Ideally, the code and the data would live together. This is the general idea behind the whole *literate programming* paradigm.

R packages process *markdown* or *Sweave* documents in the `/vignettes` directory during the build process, but that doesn't quite fit what we need to do. The vignettes may rely on some of the data they are supposed to generate, the vignette build process needs to take no more than a certain amount of time in order for packages to meet CRAN and Bioconductor guidelines, and, do you really need to generate a 50-page document detailing how the data was cleaned each time the package is built? Ideally, we'd like to separete out the data preprocessing from the build stage, but still have it be an integral part of the R package. 

## How it works

That's where `preprocessData` comes in. It enables the `R CMD preprocessData` command. Based upon `BiocCheck` developed by the BioConductor team, it installs a script (requires admin rights) into `$R_HOME/bin` that enables `R CMD preprocessData packagename` which does the following (based on ideas by Hadley Wickham and Yihui Xie):

- Looks for `datasets.R`  under the directory `packagename/data-raw` and sources it in its own environment. `datasets.R` can source other R files but this must be done via `sys.source("myotherRfile.R",env=topenv())`
- Checks the objects created by the R files.
- Computes a digest of the R objects and compares it against a DATADIGEST file in the package source directory if it exists.
- If the digests don't match, checks whether the DataVersion string in the DESCRIPTION file has been updated comapred to what is in the DATADIGEST file, enforcing bumping the data version when data changes.
- If all is well, writes a new DATADIGEST to the package source directory and writes the data objects to an .rda in the /data directory.

These `.R` file should read some raw data from a source (could be a url, or from `/inst/extdata`, tidy the data, standardize it. The `preprocessData` package takes care of saving the data to the `/data` directory, provided it 
1. Has not changed based on an md5 digest of the object.
2. If it has changed, the DataVersion (new) string has been incremented in the DESCRIPTION file.

- The package extracts roxygen2 documentation for the data objects in the .R files under "/data-raw",  and places it in "/R/packagename.R"

- `R CMD build packagename` generates `packagename_x.y.z.tar.gz` with documented and standardized data available via the usual `data()` mechanism. Unless `data-raw` is placed in `.Rbuildignore` the code to generate the data sets is distributed with the package.


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
- A file names `packagename.R` is generated under `/R` containing the roxygen2 documentation for the data sets. This overwrites any .R file with the same name already present under the `/R` directory without warning.