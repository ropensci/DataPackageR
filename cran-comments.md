## Patch 0.15.3
This is another patch release to fix continued check errors on:

* r-release-osx-x86_64
* r-patched-solaris-x86
* r-oldrel-osx-x86_64

Although pandoc is listed in the SytemRequirements, the check still throws errors. To address these I have:
* Made tests and examples conditional on the presence of pandoc using rmarkdown::pandoc_available().
* Tested the package in a simulated environment where pandoc is missing, ensuring it correctly detects a missing pandoc installation.


## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-08-14 r75143)
* local OS X install (x86_64-apple-darwin16.7.0), R 3.5.1  (2018-08-14 r75143) with pandoc missing.
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.0
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-08-08 r75087)
* Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R 3.5.1 Patched (2018-08-06 r75070)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.
* build on: Ubuntu 18.04.1 LTS x86_64-pc-linux-gnu (64-bit) (via OpenCPU CI) R version 3.5.1 (2018-07-02)

## R CMD check results

There were no ERRORs or WARNINGs 

There was 1 NOTE when pandoc was present:

NOTE
Maintainer: ‘Greg Finak <gfinak@fredhutch.org>’

Days since last update: 5

There were 2 NOTEs when pandoc was absent, the one above and :

NOTE:
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.


## Downstream dependencies

The package has no reverse dependencies.
