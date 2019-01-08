# rsofun

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)

The rsofun R package provides functions for all the routine steps of running the P-model within R.

- Setup of the model environment
- Preparation of input files
- Calibrating model parameters
- Running the model
- Reading outputs into R
- Evaluating outputs (benchmarking)

It also provides a generic function (`pmodel()`) to run alternative implementations of the P-model in different languages (Fortran using the [SOFUN](https://github.com/stineb/sofun) modelling framework, Python not implemeted yet), wrapped within R, and an impementation in R itself. 

## Installation

To install and load the rsofun package run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github( "stineb/rsofun", dependencies = NA )
library(rsofun)
```

### Dependencies

The `rsofun` package requires a large number of other R-packages (dependencies). Required dependencies are essential for `rsofun` functions and are:

- `dplyr`,`ggplot2`,`lubridate`,`Metrics`,`mgcv`,`ncdf4`,`optimr`,`purrr`,`readr`,`rlang`,`stringr`,`tidyr`, and `LSD`

Suggested dependencies are required only for certain optional tasks and are:

- `BayesianTools`, `caret`, `GenSA`, `gplots`, `hydroGOF`, `maps`, `maptools`, `neuralnet`, `nnet`, `raster`, `sp`, and `testthat`

To install locally unavailable packages, run
```r
install_dependencies_rsofun()
```

To load dependencies, run
```r
load_dependencies_rsofun()
```

Suggested dependencies are only used optionally:

- `GenSA`: used by `calib_sofun()`, if `settings_calib$method=="gensa"`, see calib_sofun.R
- `BayesianTools`: used by `calib_sofun()`, if `settings_calib$method=="BayesianTools"`, see calib_sofun.R
- `caret`: used by `gapfill_nn()`, see gapfill_nn.R and prepare_input_sofun.R 
- `neuralnet`: used by `gapfill_nn()` and `eval_response_neuralnet()`, see eval_response_neuralnet.R and gapfill_nn.R
- `nnet`: used by `gapfill_nn()` and `eval_response_neuralnet()`, see eval_response_neuralnet.R and gapfill_nn.R
- `hydroGOF`: used by `analyse_modobs()` to calculate the Nash-Sutcliffe model efficiency, see analyse_modobs.R

### External environment

Several functions require external programs called by `system()` calls from within R. Required programs need to be installed beforehand, as follows (**Note: These are not required for the "Simple usage", described below**):

- [NCO](http://nco.sourceforge.net/). Is used by `get_pointdata_()` functions (executing the Bash script `"./extract_pointdata_byfil.sh"` with NCO command `ncks`). Before using `rsofun`, install NCO on a Mac by
```sh
brew tap homebrew/science
brew install nco
```
- [CDO](https://code.mpimet.mpg.de/). Is used by the `proc_ncout_sofun_bysite()` function (executing Bash script `"./proc_output_sofun.sh"` with CDO command `cdo mergetime`). Before using `rsofun`, install CDO on a Mac by
```sh
brew install cdo
```
Apparently, CDO is no longer available as a Homebrew formula (right?). Unse MacPorts instead:
```sh
sudo port install cdo
```

## Usage

rsofun provides 

### Simple usage


### Site-scale simulations


### Spatial simulations



## Examples

Examples are given in several vignettes, also available [here](http://rpubs.com/stineb/vignette_rsofun3).
