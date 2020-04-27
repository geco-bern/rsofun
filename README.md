[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)

# rsofun

Provides a modelling framework that implements the P-model for leaf-level acclimation of photosynthesis for site-scale simulations, with SPLASH used for simulating the soil water balance (see also [Stocker et al., 2019 GMDD](https://www.geosci-model-dev-discuss.net/gmd-2019-200/)). The package provides the following functionalities:

- Calibrating model parameters
- Running the model and getting outputs directly back into R ("tidy" data)
- Evaluating outputs (benchmarking)

Model forcing and calibration data is collected using the [ingestr](https://stineb.github.io/ingestr/) package. See [here](https://rpubs.com/stineb/rsofun) for an example.

Parallelisation for a large number of site-level simulations is provided using the *multidplyr* R package. Calibration uses the *GenSA* R package.  

The P-model is implemented in different repositories for different purposes:

- **rsofun** (this package): Is for site-scale simulations (large ensemble of sites can be run in parallelised mode), forced by time series of meteorological data. Acclimation of photosynthesis is assumed at a user-defined time scale. I.e., the P-model optimality criterion (Wang et al., 2017; Prentice et al., 2014) is solved daily with "damped" daily variations in the forcing data (similar to a running mean). All model code is implemented in Fortran.
- [**rpmodel**](https://stineb.github.io/rpmodel/): Implements the same equation as rsofun (all native R), but solves the optimality criterion for each time step independently (instantaneous acclimation). This can be used for hypothesis generation, exploration, and illustrations. Transient simulations of acclimation and GPP should be done using rsofun.
- [**SOFUN**](https://stineb.github.io/sofun/): This is for P-model simulations on a (global) spatial grid and is purely in Fortran. Forcing data is read from NetCDF files and outputs are written to NetCDF files.

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/rsofun")
library(rsofun)
```

## Environment

**This is important**

In order to successfully compile the package (Fortran source), you need to have gfortran installed and manually adjust the compiler flag specification. To do so, open the Makeconf file (you'll find it by entering in R: `file.path(R.home("etc"), "Makeconf")`). In there, add the gfortran flag `-ffree-line-length-0`. The respective line then looks like this:
```sh
FCFLAGS = -Wall -g -O2 $(LTO) -ffree-line-length-0
```

## Dependencies

The `rsofun` package requires a large number of other R-packages (dependencies). Required dependencies are essential for `rsofun` functions and are:

- dplyr, purrr, lubridate, tidyr, raster, lubridate, stringi, sp, GenSA, stringr, rlang, readr

## Example

See vignette [Example for using rsofun](./articles/example.html) for how to run the model, and [Prepare rsofun forcing data](./articles/prepare_inputs_rsofun.html) for how to use the [ingestr R package ](https://stineb.github.io/ingestr/) for collecting the forcing data to run rsofun. 

## Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git. 

### Application only

I recommend the following steps if you would just like to use this package (no development):

- Install and load the library as described under 'Installation' above.

### For developers

I recommend the following steps if you would like to use and further develop the package (even if this is just some extension for your own application - Keep in mind: Others may benefit from your efforts too!):

1. Make sure you have a Github account.
2. Log on to Github, and go to [https://github.com/stineb/rsofun](https://github.com/stineb/rsofun) and click on 'Fork' in the upper right corner. This makes a copy of the repository which then belongs to you, meaning that you can modify, commit, and push changes back to your forked repository as you please.
3. Clone your fork to your local computer by entering in your terminal (here, it's cloned to a subdirectory `ingestr` placed in your home directory):
```sh
cd home
git clone https://github.com/<your_github_username>/rsofun.git
```
4. In RStudio, create a new project in your local directory `~/rsofun/`. This opens the repository in RStudio and you have access to the code where all the functions of this package are implemented (see subdirectory `./R/`).
5. In RStudio, after having edited code, select the 'Build' tab and click on 'Install and Restart' to build the package again. For quick edits and checks, you may simply source the edited files instead of re-building the whole package. If you like to add new functions, create a new source file that contains your function in subdirectory `./R/`, write a nice roxygen header (see other source files as an example), then click on 'Build' -> 'More' -> 'Document', and then again on 'Install and Restart'.
6. You can upload (commit and push) your edits and additions to your forked repository by
```sh
git add -u  # adds all edits to your next commit
git add <newfile>  # adds new file to the git repository
git commit -m "a brief description of what you did"
git push  # pushes the commit to your fork 
```
6. If you're happy with your new edits and additions to the package, you may want to have it fed back from your fork to the original repository. To do so, please create a new *pull request* in GitHub: Click on 'New pull request' on [the repository page](https://github.com/stineb/rsofun) and follow the inuitive steps. Thanks!

