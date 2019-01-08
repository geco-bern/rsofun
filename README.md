[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)
<a href="https://www.buymeacoffee.com/H2wlgqCLO" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>

# rsofun

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
Build the vignette for a more comprehensive documentation:
```r
devtools::build_vignettes()
```
Display the rsofun overview vignette:
```r
vignette("overview", "rsofun")
```

### Dependencies

The `rsofun` package requires a large number of other R-packages (dependencies). Required dependencies are essential for `rsofun` functions and are:

- `dplyr`,`ggplot2`,`lubridate`,`Metrics`,`mgcv`,`ncdf4`,`optimr`,`purrr`,`readr`,`rlang`,`stringr`,`tidyr`, `LSD`, and `GenSA`, 

Suggested dependencies are required only for certain optional tasks and are:

- `BayesianTools`, `caret`, `gplots`, `hydroGOF`, `maps`, `maptools`, `neuralnet`, `nnet`, `raster`, `sp`, and `testthat`

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

rsofun provides functionalities to run the P-model in different setups, and implementations in different languages.

### Simple usage

#### R

The P-model can be run as a simple R function. This uses model code that is directly implemented in R (in file `rpmodel.R`) For example:
```r
tc    <- 20     # deg C
ppfd  <- 800    # mol/m2/d
vpd   <- 1000   # Pa
co2   <- 400    # ppm
elv   <- 0      # m.a.s.l.
fapar <- 1      # unitless
out_pmodel_R <- rsofun::rpmodel( tc = tc, vpd = vpd, co2 = co2, elv = elv, kphio = 0.05, fapar = fapar, ppfd = ppfd, method_optci="prentice14", method_jmaxlim = "wang17", do_ftemp_kphio = FALSE )
```

This returns a named list of P-model predictions, including the following elements:

- `gammastar`: photorespiratory compensation point, (Pa)
- `kmm`: Michaelis-Menten coefficient for photosynthesis (Pa)
- `ci`: leaf-internal partial pressure, (Pa)
- `chi`: = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
- `iwue`: intrinsic water use efficiency (unitless)
- `lue`: light use efficiency (mol CO2 / mol photon)
- `gpp`: gross primary productivity (g C m-2, calculated only if fAPAR and PPFD are not 'dummy')
- `vcmax`: maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
- `vcmax25`: Vcmax25 (Vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
- `vcmax_unitfapar`: Vcmax per fAPAR (mol CO2 m-2 s-1)
- `vcmax_unitiabs`: Vcmax per unit absorbed light (xxx units)
- `rd`: Dark respiration (mol CO2 m-2 s-1)
- `rd_unitfapar`: Dark respiration per fAPAR (mol CO2 m-2 s-1)
- `rd_unitiabs`: Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
- `actnv`: Active metabolic leaf N (canopy-level), mol N/m2-ground
- `actnv_unitfapar`: Active metabolic leaf N (leaf-level, top of canopy), mol N/m2-leaf
- `actnv_unitiabs`: Active metabolic leaf N per unit absorbed light, mol N/m2/mol

#### Fortran wrapped into R

The P-model is also implemented in Fortran in the [SOFUN](https://github.com/stineb/sofun) modelling framework. We can use the generic P-model wrapper function `pmodel( ..., implementation = "Fortran" )` to run the P-model implemented in SOFUN and read its output back into R. To set up and run SOFUN, follow the steps below as an example:

1. Define the simulation settings:
```r
settings_sims_simple <- list( 
  setup = "simple", 
  implementation  = "fortran", 
  dir_sofun = "~/sofun/trunk/" 
  )
```

2. Define the model setup. Use the `demo_pmodel` compilation for the simple setup where the P-model is run as a function:
```r
setup_sofun_simple <- list(
  model      = "demo_pmodel",
  dir        = "~/sofun/trunk",
  do_compile = FALSE,
  simsuite   = FALSE
  )
```

3. Prepare the setup. For the simple setup, this clones the [SOFUN](https://github.com/stineb/sofun) repository, switches to branch `pnmodel`, and puts the executables in place (copying from the rsofun package or compiling if `do_compile = TRUE`). 
```r
settings_sims_simple <- prepare_setup_sofun( settings = settings_sims_simple )
```

4. Create parameter (text) file. In the example below, we're using an example parameter file that is provided along with the `rsofun` package. Creating the parameter file is done again in R by:
```r
params_opt <- readr::read_csv( paste0( path.package("rsofun"), "/extdata/params_opt_kphio_soilm_global.csv" ) )
nothing <- rsofun::update_params( params_opt, dir_sofun )
```

5. Now, we can wrap SOFUN in R as:
```r
out_pmodel <- pmodel( temp = 20, vpd = 100, co2 = 300, ppfd = 800, fapar = 1.0, elv = 200, implementation = "fortran", sofundir = dir_sofun )

```
This returns a named list, similar as described above for the `rpmodel()` function.


**Alternatively**, above steps can also be done directly in the shell without using the `prepare_setup_sofun()` function as follows:

1. Get the [SOFUN](https://github.com/stineb/sofun) repository and switch to branch `pnmodel`. If you don't plan to further develop the publicly available code, you may just directly clone it:
```sh
cd # This clones sofun into your home
git clone -b pnmodel https://github.com/stineb/sofun.git
```
If you plan to develop new code, fork SOFUN in github. This creates a copy of the repository for your own, where you can implement your code edits. Once you're ready to share these (this is generally very welcome!), you can create a pull request in github to merge edits back into the original repository.

2. Get the executables. Executables, compiled on a 64 bit UNIX machine are provided along with the `rsofun` package. These can just be copied into your local SOFUN directory. In R, do:
```r
system( paste0( "cp ", path.package("rsofun"), "/extdata/rundemo_pmodel dir_sofun" ) )
```
Note that `dir_sofun` is the local path where SOFUN was cloned into.

Alternatively or if you require executables on a Windows machine, compile the Fortran code. This is tested using the publicly available gfortran compiler. Make sure you have this compiler installed. See [here](http://www.lapk.org/gfortran/gfortran.php?OS=7) for a guide to install gfortran. For the simple function-like setup of the Fortran P-model wrapped into R, compile from your shell as follows:
```sh
cd dir_sofun
make demo_pmodel
```

3. Create parameter (text) file. In the example below, we're using an example parameter file that is provided along with the `rsofun` package. Creating the parameter file is done again in R by:
```r
params_opt <- readr::read_csv( paste0( path.package("rsofun"), "/extdata/params_opt_kphio_soilm_global.csv" ) )
nothing <- rsofun::update_params( params_opt, dir_sofun )
```

4. To run a quick test to see whether the Fortran-only part is running, the following command (entered in your shell) should return a sequence of numeric values:
```sh
echo 20 100 300 800 1.0 200 | ./rundemo_pmodel  # the values are: temp, vpd, co2, ppfd, fapar, elv
# should return:
#    3.26343822       45.5780487       26.0814800      0.877946496       2.28850331E-05  0.230092600       0.00000000       0.00000000       184.074081       48.1304512       75.5490265       48.1304512       6.01630621E-02  0.637075663      0.674691319      0.713611901      0.713611901       8.92014883E-04   25412.6855       25412.6855       31.7658558       0.00000000    
```

5. Now, we can wrap SOFUN in R as:
```r
out_pmodel <- pmodel( temp = 20, vpd = 100, co2 = 300, ppfd = 800, fapar = 1.0, elv = 200, implementation = "fortran", sofundir = dir_sofun )

```
This returns a named list, similar as described above for the `rpmodel()` function.


### Site-scale simulations

See overview vignette:
```r
vignette("overview", "rsofun")
```

### Spatial simulations

See overview vignette:
```r
vignette("overview", "rsofun")
```

## Tests
Display the rsofun test vignette:
```r
vignette("test_implementations", "rsofun")
```

## Examples

Examples are given in several vignettes, also available [here](http://rpubs.com/stineb/vignette_rsofun3).

## Acknowledgements

The main author (B. Stocker) was funded by Marie Sklodowska-Curie fellowship H2020-MSCA-IF-2015, project FIBER, grant number 701329.
