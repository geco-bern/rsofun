# rsofun

This repository contains the code for the R-package that provides functions for all the routine steps in running the SOFUN model:

- Setup of the model environment
- Preparation of input files
- Calibrating model parameters
- Running the model
- Reading outputs into R
- Evaluating outputs (benchmarking)

Get this R-package by 
```sh
install_github("stineb/rsofun")
```

## Dependencies

### R environment

Note that the package `rsofun` requires a large number of other R-packages (dependencies). Some are required only for specific tasks (e.g. creating certain plots or applying certain calibration methods). These are loaded optionally (XXX TODO XXX). Others are essential as the source code is generally implemented adopting `tidyverse` syntax. Essential dependencies need to be installed beforehand and are loaded with the `library(rsofun)` call. These are:

- `dplyr`
- `tidyr`
- `purrr`
- `readr`
- `rlang`
- `stringr`
- `ggplot2`
- `lubridate`
- `Metrics`
- `mgcv`
- `ncdf4`
- `optimr`

Suggested dependencies are:

- `GenSA`: used by `calib_sofun()`, if `settings_calib$method=="gensa"`, see calib_sofun.R
- `BayesianTools`: used by `calib_sofun()`, if `settings_calib$method=="BayesianTools"`, see calib_sofun.R
- `caret`: used by `gapfill_nn()`, see gapfill_nn.R and prepare_input_sofun.R 
- `neuralnet`: used by `gapfill_nn()` and `eval_response_neuralnet()`, see eval_response_neuralnet.R and gapfill_nn.R
- `nnet`: used by `gapfill_nn()` and `eval_response_neuralnet()`, see eval_response_neuralnet.R and gapfill_nn.R
- `hydroGOF`: used by `analyse_modobs()` to calculate the Nash-Sutcliffe model efficiency, see analyse_modobs.R

Install required dependencies by:
```r
list_of_packages <- c( "dplyr", "tidyr", "purrr", "readr", "rlang", "stringr", 
                       "ggplot2", "lubridate", "Metrics", "mgcv", "ncdf4", "optimr" 
                       )
new_packages <- list_of_packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if( length(new_packages)>0 ) install.packages(new_packages)
```

### External environment

Several functions require external programs called by `system()` calls from within R. Required programs need to be installed beforehand, as follows:

- [NCO](http://nco.sourceforge.net/). Is used by `get_pointdata_()` functions (executing the Bash script `"./extract_pointdata_byfil.sh"` with NCO command `ncks`). Before using `rsofun`, install NCO on a Mac by
```sh
brew tap homebrew/science
brew install cdo
```
- [CDO](https://code.mpimet.mpg.de/). Is used by the `proc_ncout_sofun_bysite()` function (executing Bash script `"./proc_output_sofun.sh"` with CDO command `cdo mergetime`). Before using `rsofun`, install CDO on a Mac by
```sh
brew install cdo
```

## Examples

Examples are given in several vignettes.