## Test environments
* local OS X install (x86_64-apple-darwin16.7.0), R 3.6.0  (2018-08-14 r75143)
* ubuntu  14.04.05 LTS (on travis-ci) R 3.5.0
* ubuntu  14.04.05 LTS (on travis-ci) R Under development (unstable) (2018-08-08 r75087)
* Windows Server 2012 R2 x64 (build 9600) (under Appveyor) R 3.5.1 Patched (2018-08-06 r75070)
* Windows (via Winbuilder) R 3.5.1 R-release and  3.6.0 R-devel.

## R CMD check results

There were no ERRORs or WARNINGs 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Greg Finak <gfinak@fredhutch.org>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  CMD (15:41)
  github (22:25)
  reproducibility (23:29)

The above is a false positive.


## Downstream dependencies

The package has no reverse dependencies.
