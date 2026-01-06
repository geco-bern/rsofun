# Data format

## Philosophy

Overall, the package uses the {tidyverse} data paradigm (Wickham, 2017),
using nested data frames (tibbles) to store model input and output, and
validation data. Where possible the package uses a consistent ontogeny
in terms of variables and data structures used.

## Drivers data (inputs)

A drivers object is used as unique input to the models, consisting of a
list of sites. Each site (i.e. row) contains the following data: -
`sitename`: site name - `site_info`: location specific site information
(nested tibble) - `params_siml`: simulation parameter settings (nested
tibble) - `forcing`: environmental forcing data (nested tibble)

Each of the provided demo drivers contain only one sample site:

``` r
# call to the included p-model demo drivers
rsofun::p_model_drivers
#> # A tibble: 1 × 4
#>   sitename params_siml       site_info        forcing              
#>   <chr>    <list>            <list>           <list>               
#> 1 FR-Pue   <tibble [1 × 11]> <tibble [1 × 4]> <tibble [2,190 × 13]>

# call to the included BiomeE (p-model) demo drivers
rsofun::biomee_p_model_drivers
#> # A tibble: 1 × 8
#>   sitename site_info         params_siml params_tile params_species init_cohort
#>   <chr>    <list>            <list>      <list>      <list>         <list>     
#> 1 CH-Lae   <tibble [1 × 10]> <tibble>    <tibble>    <tibble>       <tibble>   
#> # ℹ 2 more variables: init_soil <list>, forcing <list>

# call to the included BiomeE (gs leuning) demo drivers
rsofun::biomee_gs_leuning_drivers
#> # A tibble: 1 × 8
#>   sitename site_info         params_siml params_tile params_species init_cohort
#>   <chr>    <list>            <list>      <list>      <list>         <list>     
#> 1 CH-Lae   <tibble [1 × 10]> <tibble>    <tibble>    <tibble>       <tibble>   
#> # ℹ 2 more variables: init_soil <list>, forcing <list>
```

Nested data structures can be accessed like this:

``` r
# Accessing the site information for the first site
rsofun::biomee_gs_leuning_drivers$site_info[[1]]
#> # A tibble: 1 × 10
#>     lon   lat   elv year_start year_end c4    igbp_land_use
#>   <dbl> <dbl> <dbl>      <dbl>    <dbl> <lgl> <chr>        
#> 1  8.36  47.5   700       2004     2014 FALSE Mixed Forests
#> # ℹ 3 more variables: plant_functional_type <chr>, date_start <date>,
#> #   date_end <date>
```

### Specific data for each model

Each model has its own specificities when it comes to the set of
simulation parameters and forcing data. Please refer to the
documentation for each model for an exhaustive list of parameters and
data required by the model.

- P-model:
  [`?run_pmodel_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_pmodel_f_bysite.md)
- BiomeE:
  [`?run_biomee_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_biomee_f_bysite.md)

### Forcing data

The forcing data contains environmental variables commonly available at
fluxnet (reference) or ICOS atmospheric gas exchange measurement
locations or gathered from various gridded or re-analysis sources.
Forcing data are expected to be sequences of complete years (each
starting on January 1st and ending on december 31st), where leap days
are excluded (February 29th should not be present).

Each model has a specific forcing data resolution: - P-model: daily -
BiomeE (p-model): daily - BiomeE (gs leuning): hourly

Forcing data present in the demo drivers can be used as examples:

``` r
# Detailed look at the forcing data for the P-model
rsofun::p_model_drivers$forcing[[1]]
#> # A tibble: 2,190 × 13
#>    date        temp   vpd    ppfd netrad    patm  snow    rain  tmin  tmax fapar
#>    <date>     <dbl> <dbl>   <dbl>  <dbl>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>
#>  1 2007-01-01 10.0  183.  1.06e-4   4.17  99944.     0 2.55e-5  7.12 13.0  0.605
#>  2 2007-01-02  8.42 417.  1.92e-4 -22.2   99992.     0 6.94e-6  6.79  9.33 0.603
#>  3 2007-01-03  9.13 566.  1.87e-4 -16.6  100075      0 0        4.21 11.2  0.600
#>  4 2007-01-04 10.1  375.  8.28e-5 -16.8   99338.     0 0        3.96 12.2  0.598
#>  5 2007-01-05 10.7  508.  1.83e-4  -6.60  99419.     0 0        9.26 11.4  0.596
#>  6 2007-01-06 13.6  656.  1.88e-4 -11.2   99467.     0 0        8.96 15.4  0.596
#>  7 2007-01-07 13.7  567.  1.55e-4   7.32  99252.     0 0        9.55 15.6  0.597
#>  8 2007-01-08 10.7  273.  7.17e-5  -3.00  98871.     0 0        7.04 12.0  0.597
#>  9 2007-01-09 16.4  682.  1.88e-4   9.94  99067.     0 0        9.46 19.0  0.598
#> 10 2007-01-10 11.4   26.0 2.77e-5  -6.41  99444.     0 0        8.98 12.0  0.598
#> # ℹ 2,180 more rows
#> # ℹ 2 more variables: co2 <dbl>, ccov <dbl>
```

## Output data

The output of the model contains one line per site found in the
drivers: - `sitename`: site name - `data`: output data for the site
(nested tibble)

The data structure for the output `data` is specific for each model.

### P-model

For detailed information about the content of `data`, see
[`?run_pmodel_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_pmodel_f_bysite.md).

### BiomeE

`data` contains the following tables: - `output_daily_tile`: Daily
output during the simulated period - `output_annual_cohorts`: Annual
output at the cohort level during the simulated period -
`output_annual_tile`: Yearly output during the spin-up and simulated
periods.

For detailed information about each table, see
[`?run_biomee_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_biomee_f_bysite.md).
