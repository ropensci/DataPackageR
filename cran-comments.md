## Submission 0.15.8
* Fix a test associated with usethis 1.5.1.9000


## Test environments
* Local linux (x86_64-pc-linux-gnu), R 3.6.1  (2019-07-05)
* Local rhub docker (rhub/ubuntu-gcc-devel)
* Local rhub docker (rhub/ubuntu-gcc-release)
* Local rhub docker (rhub/ubuntu-rchk)
* Windows 10 enterprise v1607 14393.2906 (R 4.0.0 2020-04-09 v78186)
* rhub macOS 10.11 El Capitan, R-release (experimental)
* rhub fedora-gcc-devel

## R CMD check results

There were no ERRORs or WARNINGs

One NOTE
─  checking CRAN incoming feasibility ... Note_to_CRAN_maintainers (1.4s)
   Maintainer: 'Greg Finak <gfinak@fredhutch.org>'

On rhub/fedora-gcc-devel one test fails because the texlive-framed package is not installed and is needed for rmarkdown.

I observe two test failures on rhub windows systems that I am unable to reproduce on windows systems I have avaialble to me with R 4.0.0. 

## Downstream dependencies

The package has no reverse dependencies.
