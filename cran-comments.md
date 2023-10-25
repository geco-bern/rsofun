Dear CRAN team,

This is the re-submission of the {rsofun} package. We have addressed the following
concerns voiced by Benjamin Altman:

- Corrected the software naming in package title and description.
- Added a \value field to all the exported functions, explaining the object returned
  by each function.
- Removed \dontrun statements in the documentation examples, now \donttest
  is used for examples that have long runtimes.
- Included license and copyright statements in a COPYING file. The original authors
  of the software borrowed for this package were contacted and permissions to use
  parts of their code were discussed, as well as the copyright and license that
  accompany their code. The direct authors of this package are listed in the
  DESCRIPTION file.

---

The {rsofun} package provides the implementation of a modelling framework for site-scale simulations of ecosystem processes, with low level routines in Fortran 90. It contains the following models:
- P-model for leaf-level acclimation of photosynthesis from Stocker et al. (2019).
- SPLASH for bioclimatic variables, including the surface radiation budget and the soil water balance from Davis et al. (2017).
- BiomeE for comprehensive simulations of ecosystem carbon and water cycling, tree growth, and tree cohort-explicit forest dynamics following the Perfect Plasticity Approximation, from Weng et al. (2015).

This package is an extension of {rpmodel} in the sense that it expands the P-model implementation and provides functions for multiple-site simulations and model parameter calibration. 

The full documentation can be found at the github repository link: https://geco-bern.github.io/rsofun

Code coverage sits at ~72%, with remaining uncovered code pertaining to minor input data format checks of the main functions. The underlying P-model implementation is based on the {rpmodel} and the parameter calibration routines use packages {GenSA} and {BayesianTools}.

I hope this package is useful for other earth system scientists and the larger CRAN community. Kind regards, Josefa Arán.

---

I have read and agree to the CRAN policies enumerated here: https://cran.r-project.org/web/packages/policies.html

## Local, github actions and r-hub checks

- Pop!_OS 22.04 install on R 4.3

- Ubuntu 22.04, MacOS and Windows on github actions (devel / release)

- rhub::check_on_cran() with only notes for latex elements

- codecove.io code coverage at ~72%

## Github actions R CMD check results

0 errors | 0 warnings | 0 notes
