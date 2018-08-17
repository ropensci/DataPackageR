## Patch
This is a patch release to fix check errors on:

* r-devel-windows-ix86+x86_64
* r-patched-solaris-x86

The errors were related to a missing pandoc installation, and in this version I have:
* added pandoc to the SystemRequirements in the DESCRIPTION.

Note: I did not get these errors on R-devel via winbuilder below.

## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-08-14 r75143)
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.0
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-08-08 r75087)
* Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R 3.5.1 Patched (2018-08-06 r75070)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.
* build on: Ubuntu 18.04.1 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R version 3.5.1 (2018-07-02)

## R CMD check results

There were no ERRORs or WARNINGs 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Greg Finak <gfinak@fredhutch.org>’

Days since last update: 1

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.5281/zenodo.1292095
    From: README.md
    Status: 404
    Message: Not Found

- The zenodo site is down at the time of package testing. 

## Downstream dependencies

The package has no reverse dependencies.
