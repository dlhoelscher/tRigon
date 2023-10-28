## R CMD check results

-- R CMD check results -------------------------------------------- tRigon 0.3.2 ----
Duration: 4m 50.5s

checking package dependencies ... NOTE
Imports includes 22 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally.

0 errors v | 0 warnings v | 1 note x

##Win Builder results
* using log directory 'd:/RCompile/CRANguest/R-devel/tRigon.Rcheck'
* using R Under development (unstable) (2023-10-27 r85420 ucrt)
* using platform: x86_64-w64-mingw32
* R was compiled by
    gcc.exe (GCC) 12.3.0
    GNU Fortran (GCC) 12.3.0
* running under: Windows Server 2022 x64 (build 20348)
* using session charset: UTF-8
* checking for file 'tRigon/DESCRIPTION' ... OK
* this is package 'tRigon' version '0.3.2'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Hoelscher <dhoelscher@ukaachen.de>'

New submission

Possibly misspelled words in DESCRIPTION:
  Pathomics (2:32)
  omics (18:52)
  pathomics (18:41)
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking serialization versions ... OK
* checking whether package 'tRigon' can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking 'build' directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [20s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from 'inst/doc' ... OK
* checking files in 'vignettes' ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in 'inst/doc' ... OK
* checking re-building of vignette outputs ... OK
* checking PDF version of manual ... [22s] OK
* checking HTML version of manual ... OK
* DONE
Status: 1 NOTE
