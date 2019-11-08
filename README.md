[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/stineb/rsofun?branch=master&svg=true)](https://ci.appveyor.com/project/stineb/rsofun)
<a href="https://www.buymeacoffee.com/H2wlgqCLO" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>

# rsofun

Provides a wrapper for the SOFUN model implemented in Fortran. Shared memory (calling Fortran functions from within R) increases speed and facilitates use and installation. The package provides the following functionalities:

- Collecting input data for a large number of site-scale simulations
- Calibrating model parameters
- Running the model and getting outputs directly back into R (tidy data)
- Evaluating outputs (benchmarking)


## Installation

To install and load the rsofun package run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github( "stineb/rsofun@v0.2" )
library(rsofun)
```

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

```r
library(dplyr)
library(rsofun)
load_dependencies_rsofun()
```

### Simulation settings

Make sure we have a site meta info file containing information about longitude, latitude, elevation, years of data availability, etc. Use the FLUXNET 2015 meta info data provided with rsofun.

```r
path_siteinfo <- "~/.siteinfo_example_fortran.csv"
siteinfo <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  dplyr::filter(sitename %in% c("FR-Pue", "FR-LBr", "IT-Noe")) %>%
  write_csv(path = path_siteinfo)
```

Define the simulation parameters.
```r
params_siml <- list(
  spinup         = TRUE,
	spinupyears    = 10,
	recycle        = 1,
	soilmstress               = TRUE,
	tempstress                = TRUE,
	in_ppfd                   = TRUE,
	in_netrad                 = FALSE,
	const_clim_year           = -9999,
	const_lu_year             = -9999,
	const_co2_year            = -9999,
	const_ndep_year           = -9999,
	const_nfert_year          = -9999,
	daily_out_startyr         = 1982,
	daily_out_endyr           = 1982,
	outdt                     = 1,
	ltre                      = FALSE,
	ltne                      = FALSE,
	ltrd                      = FALSE,
	ltnd                      = FALSE,
	lgr3                      = TRUE,
	lgn3                      = FALSE,
	lgr4                      = FALSE,
	loutplant                 = FALSE,
	loutgpp                   = FALSE,
	loutwaterbal              = FALSE,
	loutforcing               = FALSE,
	loutdgpp                  = FALSE,
	loutdrd                   = FALSE,
	loutdtransp               = FALSE,
	loutdwcont                = FALSE,
	loutdaet                  = FALSE,
	loutdpet                  = FALSE,
	loutdnetrad               = FALSE,
	loutdwbal                 = FALSE,
	loutdtemp                 = FALSE,
	loutdfapar                = FALSE,
	loutdtemp_soil            = FALSE,
	lcalibgpp                 = FALSE,
	lcalibfapar               = FALSE,
	lcalibtransp              = FALSE,
	lcaliblatenth             = FALSE
	)

settings_sims <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)
```


### Define model parameters

```r
params_modl <- list(
	kphio           = 0.06,
	soilm_par_a     = 0.1,
	soilm_par_b     = 0.7,
	vpdstress_par_a = 0.2,
	vpdstress_par_b = 0.2,
	vpdstress_par_m = 5
	)
```

### Define soil parameters

```r
list_soiltexture <- list(
  top = list(fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = list(fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)
```

### Get input

First, define input settings.
```r
settings_input <-  list(
    data                     = NA,
    temperature              = "fluxnet2015",
    precipitation            = "fluxnet2015",
    vpd                      = "fluxnet2015",
    ppfd                     = "fluxnet2015",
    netrad                   = "fluxnet2015",  #  c("fluxnet2015", "watch_wfdei"),
    patm                     = "fluxnet2015",
    netrad                   = NA,
    cloudcover               = "cru",
    path_input               = "~/sofun_inputs/example/",
    path_watch_wfdei         = "~/data/watch_wfdei/",
    path_cru                 = "~/data/cru/ts_4.01/",
    path_MODIS_FPAR_MCD15A3H = "~/data/fluxnet_subsets/fapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_fluxnet2015_gee_subset/",
    path_MODIS_EVI_MOD13Q1   = "~/data/fluxnet_subsets/fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_fluxnet2015_gee_subset/",
    path_co2                 = "~/data/co2/cCO2_rcp85_const850-1765.csv",
    path_fluxnet2015         = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
    path_fluxnet2015_hh      = "~/data/FLUXNET-2015_Tier1/20191024/HH/",
    get_from_remote          = FALSE,
    settings_gee             = get_settings_gee( 
      bundle = "fpar", 
      python_path = "/Users/benjaminstocker/Library/Enthought/Canopy_64bit/User/bin/python",
      gee_path = "~/gee_subset/gee_subset/"
      ),
  fapar = "MODIS_FPAR_MCD15A3H",
  splined_fapar = TRUE
  )
```

Then, get the input data.
```r
ddf_input <- prepare_input_sofun(
  settings_input = settings_input,
  settings_sims = settings_sims,
  overwrite_csv_climate_lev1 = FALSE,
  overwrite_csv_climate_lev2 = FALSE,
  overwrite_csv_climate_lev3 = TRUE,
  overwrite_rdata_climate = TRUE,
  overwrite_csv_fapar = FALSE,
  verbose = FALSE
  )
```

### Run the model

```r
## single site simulation
mod <- run_sofun_f_bysite( 
      settings         = dplyr::filter(settings_sims, sitename == settings_sims$sitename[1]), 
      params_modl      = params_modl, 
      list_soiltexture = list_soiltexture,
      forcing          = dplyr::filter(ddf_input, sitename == settings_sims$sitename[1])
      )
      
## call SOFUN for multiple sites
ptm <- proc.time()
mod <- runread_sofun_f( 
  settings = settings_sims, 
  ddf_input = ddf_input, 
  list_soiltexture = list_soiltexture, 
  params_modl = params_modl 
  )
ptm <- proc.time() - ptm
print(ptm)

mod$daily[[1]] %>% 
  ggplot(aes(x=date, y=gpp)) +
  geom_line() + 
  labs(title = names(mod$daily[1]), subtitle = "SOFUN output")
```


## Acknowledgements

The main author (B. Stocker) was funded by Marie Sklodowska-Curie fellowship H2020-MSCA-IF-2015, project FIBER, grant number 701329.
