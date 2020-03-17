[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)

# rsofun

Provides a wrapper for the SOFUN model implemented in Fortran. Shared memory (calling Fortran functions from within R) increases speed and facilitates use and installation. The package provides the following functionalities:

- Calibrating model parameters
- Running the model and getting outputs directly back into R ("tidy" data)
- Evaluating outputs (benchmarking)

So far, rsofun implements the P-model ([Stocker et al., 2019 GMDD](https://www.geosci-model-dev-discuss.net/gmd-2019-200/)) and was used for simulations presented in [Stocker et al., 2019 GMDD](https://www.geosci-model-dev-discuss.net/gmd-2019-200/). Input data, used as model forcing, is collected using the [ingestr](https://stineb.github.io/ingestr/) package.

Parallelisation for a large number of site-level simulations is provided using the *multidplyr* R package. Calibration uses the *GenSA* R package. 

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github( "stineb/rsofun@v1.1" )
library(rsofun)
```
Check *Releases* for the latest version. In above example, the latest is `v1.1`. Replace the respective string accordingly in the `install_github()` call above.

## Environment

**This is important**

In order to successfully compile the package (Fortran source), you need to have gfortran installed and manually adjust the compiler flag specification. To do so, open the Makeconf file (you'll find it by entering in R: `file.path(R.home("etc"), "Makeconf")`). In there, add the gfortran flag `-ffree-line-length-0`. The respective line then looks like this:
```sh
FCFLAGS = -Wall -g -O2 $(LTO) -ffree-line-length-0
```

### Dependencies

The `rsofun` package requires a large number of other R-packages (dependencies). Required dependencies are essential for `rsofun` functions and are:

- dplyr, purrr, lubridate, tidyr, raster, lubridate, stringi, sp, GenSA, stringr, rlang, readr

## Example

See [here](https://rpubs.com/stineb/rsofun) for an example.

### Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git. 

I recommend the following steps if you would just like to use this package (no development):

- In RStudio, do install and load the library (see 'Installation' above).

I recommend the following steps if you would like to use and further develop the package (even just for your own application - But keep in mind: others may benefit from your efforts too!):

1. Make sure you have a Github account.
2. Log on to Github, and go to [https://github.com/stineb/rsofun](https://github.com/stineb/rsofun) and click on 'Fork' in the upper right corner. This makes a copy of the repository that belongs to you, meaning that you can modify, commit, and push changes back to your forked repository as you please.
3. Clone your fork to your local computer by entering in your terminal (here, it's cloned to a subdirectory `ingestr` placed in your home directory):
```sh
cd home
git clone https://github.com/<your_github_username>/rsofun.git
```
4. In RStudio, create a new project in your local directory `~/rsofun/`. This opens the repository in RStudio and you have access to the code where all ingestr-functions are implemented (see subdirectory `./R/`).
5. In RStudio, after having edited code, select the 'Build' tab and click on 'Install and Restart' to build the package again. For quick edits and checks, you may simply source the edited files instead of re-building the whole package. If you like to add new functions, create new a source file in subdirectory `./R/`, write a nice roxygen header (see other source files as an example), then click on 'Build' -> 'More' -> 'Document', and then again on 'Install and Restart'.
6. If you're happy with your new edits and additions to the package, you may want to have it fet back to the original repository. To do so, please create a new *pull request* in GitHub: Click on 'New pull request' on [the repository page](https://github.com/stineb/rsofun) and follow the inuitive steps. Thanks!

