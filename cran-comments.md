## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:
* checking for future file timestamps ... NOTE unable to verify current time

## Resubmission

This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to "Penalised Likelihood for Survival Analysis with Dependent Censoring".
* For each function in R folder, replaced "import copula" by "importFrom copula indepCopula claytonCopula gumbelCopula frankCopula iTau rCopula".
* Deleted loading the packages that are imported in the R functions for the examples and testthat file.
* Deleted par() and replaced with lines() for the plots in the examples of the R functions, testthat file and README.Rmd file.
* Delete the for loops in the examples of R functions.
* Replace "=" by "<-" for the R function files, the README.Rmd file and the testthat file.
* Delete package methods from imports in DESCRIPTION file.
* Add @importFrom graphics plot for the R function plot.coxph_mpl_dc.R.
* Change the version to 0.1.1.
