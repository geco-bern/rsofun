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
devtools::install_github( "stineb/rsofun@v0.4" )
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
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = FALSE,
  tempstress         = FALSE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  const_clim_year    = -9999,
  const_lu_year      = -9999,
  const_co2_year     = -9999,
  const_ndep_year    = -9999,
  const_nfert_year   = -9999,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
  )
```

Run `prepare_setup_sofun()` to define the simulation settings that contain all the information specified by the two steps above (meta info, and simulation parameters).
```r
settings_sims <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)
```

### Define model parameters

First, let's do it by hand (calibration of parameters is shown later).
```r
params_modl <- list(
  kphio           = 0.04997714009213085,
  soilm_par_a     = 1.0,
  soilm_par_b     = 0.0,
  vpdstress_par_a = 0.2,
  vpdstress_par_b = 0.2,
  vpdstress_par_m = 5
  )
```

### Define soil parameters

For now, this is implemented as an illustration. Should be made site-specific.
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
      bundle      = "fpar", 
      python_path = "/Users/benjaminstocker/Library/Enthought/Canopy_64bit/User/bin/python",
      gee_path    = "~/gee_subset/gee_subset/"
      ),
  fapar = "MODIS_FPAR_MCD15A3H",
  splined_fapar = TRUE
  )
```

Then, get the input data.
```r
ddf_input <- prepare_input_sofun(
  settings_input             = settings_input,
  settings_sims              = settings_sims,
  overwrite_csv_climate_lev1 = FALSE,
  overwrite_csv_climate_lev2 = TRUE,
  overwrite_csv_climate_lev3 = TRUE,
  overwrite_rdata_climate    = TRUE,
  overwrite_csv_fapar        = FALSE,
  verbose                    = FALSE
  )
```

### Run the model

Run the model for all the sites specified in the first step.
```r
df_drivers <- collect_drivers_sofun( 
  settings       = settings_sims, 
  forcing        = ddf_input, 
  df_soiltexture = df_soiltexture
  )

## by spelling out arguments
mod <- run_sofun_f_bysite( 
  df_drivers$sitename[1], 
  df_drivers$params_siml[[1]], 
  df_drivers$siteinfo[[1]], 
  df_drivers$forcing[[1]], 
  df_drivers$df_soiltexture[[1]], 
  params_modl = params_modl, 
  makecheck = TRUE 
  )

## The advantage of using the nested data frame 'df_drivers' is that the
## function 'run_sofun_f_bysite' can be applied to each row using 'pmap'
## Doing it like this, the outputs are stored in a new column 'out_sofun'.
## This same code is implemented in 'run_sofun_f'
ptm <- proc.time()
df_output <- df_drivers %>%
  mutate(out_sofun = purrr::pmap(
    .,
    run_sofun_f_bysite,
    params_modl = params_modl,
    makecheck = TRUE
    )) %>%
    dplyr::select(sitename, out_sofun)
ptm <- proc.time() - ptm
print(ptm)

df_output$out_sofun[[1]] %>% 
  ggplot(aes(x=date, y=gpp)) +
  geom_line() + 
  labs(title = df_output$sitename[[1]], subtitle = "SOFUN output")
```

### Calibrate

Define calibration settings.
```r
## Define calibration settings common for all setups
settings_calib <- list(
  method              = "gensa",
  targetvars          = c("gpp"),
  timescale           = list( gpp = "d" ),
  path_fluxnet2015    = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  path_fluxnet2015_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/",
  threshold_GPP       = 0.5,
  path_gepisat        = "~/data/gepisat/v3_fluxnet2015/daily_gpp/",
  maxit               = 5, # (5 for gensa) (30 for optimr)    #
  sitenames           = mysites,
  filter_temp_max     = 35.0,
  filter_drought      = FALSE,
  metric              = "rmse",
  dir_results         = "./",
  name                = "ORG",
  par                 = list( kphio = list( lower=0.03, upper=0.07, init=0.0496 ) ),
  datasource          = list( gpp = "fluxnet2015_NT" ),
  filter_temp_min     = NA,
  filter_soilm_min    = NA
 )
```
 
Get calibration target data.
```r
ddf_obs_calib <- get_obs_calib( 
  settings_calib = settings_calib, 
  dplyr::select(df_drivers, sitename, siteinfo) %>% tidyr::unnest(siteinfo), 
  settings_input
  )
```

Calibrate the model.

```r
set.seed(1982)
settings_calib <- calib_sofun( 
  settings_calib, 
  df_drivers, 
  ddf_obs = ddf_obs_calib 
  )
```

The calibrated parameters are returned by `calib_sofun()` as part of the list:
```r
print(settings_calib$par_opt)
```


### Evaluate

Run the model once again with these parameters and evaluate results.
```r
mylist <- readr::read_csv("~/eval_pmodel/myselect_fluxnet2015.csv") %>% 
  dplyr::filter( use==1 ) %>% 
  dplyr::pull( Site )

settings_eval <- list(
  sitenames           = settings_sims$sitename,
  sitenames_siteplots = mylist,
  agg                 = 8,
  path_fluxnet2015_d  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/",
  path_fluxnet2015_w  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/",
  path_fluxnet2015_m  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/",
  path_fluxnet2015_y  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/",
  path_gepisat_d      = "~/data/gepisat/v3_fluxnet2015/daily_gpp/",
  benchmark           = list( gpp = c("fluxnet2015_NT") ),
  remove_premodis     = TRUE
  )
```

Get evaluation data (benchmarking data).
```r
filn <- "./obs_eval.Rdata"
if (file.exists(filn)){
  load(filn)
} else {
  obs_eval  <- get_obs_eval( 
    settings_eval = settings_eval, 
    settings_sims = settings_sims, 
    overwrite     = TRUE, 
    light         = TRUE,
    add_forcing   = FALSE
  )
  save(obs_eval, file = filn)
} 
```

Now run the model with calibrated parameters.
```r
params_modl <- list(
  kphio           = settings_calib$par_opt[["kphio"]],
  soilm_par_a     = 1.0,
  soilm_par_b     = 0.0,
  vpdstress_par_a = 0.2,
  vpdstress_par_b = 0.2,
  vpdstress_par_m = 5
  )

mod <- runread_sofun_f(
  df_drivers, 
  params_modl = params_modl, 
  makecheck = TRUE
  ) %>% 
  rename(id = sitename) %>% 
  unnest(out_sofun)
```

And finally do the evaluation.
```{r warning=FALSE, message=FALSE}
out_eval <- eval_sofun( 
  mod, 
  settings_eval, 
  settings_sims, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = FALSE 
  )
```

Print some results.
```r
out_eval$gpp$fluxnet2015$metrics$xdaily_pooled
```

```r
out_eval$gpp$fluxnet2015$data$xdf %>% rbeni::analyse_modobs2("mod", "obs", type = "heat")
```

```r
out_eval$gpp$fluxnet2015$data$ddf %>% 
  dplyr::filter(sitename=="BE-Vie" & year(date) < 2005) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs), col="black") +
  geom_line(aes(y = mod), col="red") + 
  labs(title = "BE-Vie")

out_eval$gpp$fluxnet2015$data$ddf %>% 
  dplyr::filter(sitename=="AU-Dry" & year(date) > 2010) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs), col="black") +
  geom_line(aes(y = mod), col="red") + 
  labs(title = "AU-Dry")
```

## Acknowledgements

The main author (B. Stocker) was funded by Marie Sklodowska-Curie fellowship H2020-MSCA-IF-2015, project FIBER, grant number 701329.
