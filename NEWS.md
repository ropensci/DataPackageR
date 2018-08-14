# DataPackageR 0.15.0
- Prepare for CRAN submission.


# DataPackageR 0.14.9

- Moving towards ropensci compliance
- NEWS.md updated with description of changes to data sets when version is bumped (or new package is created).
- Output of "next steps" for user when pakcage is built
- New `document()` function to rebuild docs from `documentation.R` in `data-raw` without rebuilding the whole package.
- Improved package test.
- R scripts processed properly into vignettes.
- Packages installed and loaded after build to make vignettes and data sets accessible in same R session.
- 

# DataPackageR 0.13.6

- Added a NEWS file.
- Cleaned up the examples.
- Snake case for all exported functions.

# DataPackageR 0.13.3

- Added the `render_root` property to the YAML configuration. Specifies where `render()` processing is done, instead of the `data-raw` directory.
