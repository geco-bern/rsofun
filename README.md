[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)
<a href="https://www.buymeacoffee.com/H2wlgqCLO" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>

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
devtools::install_github( "stineb/rsofun@v1.0" )
library(rsofun)
```
Check *Releases* for the latest version. In above example, the latest is `v1.0`. Replace the respective string accordingly in the `install_github()` call above.

## Environment

**This is important**

In order to successfully compile the package (Fortran source), you need to have gfortran installed and manually adjust the compiler flag specification. To do so, open the Makeconf file (you'll find it by entering in R: `file.path(R.home("etc"), "Makeconf")`). In there, add the gfortran flag `-ffree-line-length-0`. The respective line then looks like this:
```sh
FCFLAGS = -Wall -g -O2 $(LTO) -ffree-line-length-0
```

### Dependencies

The `rsofun` package requires a large number of other R-packages (dependencies). Required dependencies are essential for `rsofun` functions and are:

- `dplyr`,`ggplot2`,`lubridate`,`Metrics`,`mgcv`,`ncdf4`,`optimr`,`purrr`,`readr`,`rlang`,`stringr`,`tidyr`, `LSD`, and `GenSA`, 

To load dependencies, run
```r
load_dependencies_rsofun()
```

## Example run

See [here](https://rpubs.com/stineb/rsofun) for an example.

