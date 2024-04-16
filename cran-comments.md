## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

❯ checking CRAN incoming feasibility ... [12s/44s] NOTE
  Maintainer: ‘Dave Slager <dslager@scharp.org>’

  New submission

  Package was archived on CRAN

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-05-17 as email to the maintainer is
      undeliverable.

Previous maintainer changed employers, so email had been undeliverable.

## revdepcheck results

This package has no reverse dependencies.

## CRAN resubmission comments:

Thank you for the feedback which helped improve this package:

- Return values now documented for exported functions

- Occurrences of `dontrun{}` now changed to `donttest{}`

- Console output now easily suppressed with `options(DataPackageR_verbose = FALSE)`

- `DataPackageR` contains interactive functions designed and documented to change files in the user's current working directory with user consent, in an interactive R package development framework, like `?utils::package.skeleton()` and `?devtools::document()`.

- Examples and tests write files to `tempdir()`

- `options(warn = -1)` is now removed
