# rsofun P-model GPP validation data

Small example dataset of target observations (daily GPP flux data) to
optimize model parameters with the function
[`calib_sofun`](https://geco-bern.github.io/rsofun/dev/reference/calib_sofun.md)

## Usage

``` r
p_model_validation
```

## Format

A tibble of validation data:

- sitename:

  A character string containing the site name (e.g. 'FR-Pue').

- data:

  A tibble \[ 2,920 x 3 \] with time series for the following variables:

  date

  :   Date vector with format YYYY-MM-DD.

  gpp

  :   The observed Gross Primary Productivity (GPP) for each time stamp
      (in gC m\\^{-2}\\ d\\^{-1}\\).

  gpp_unc

  :   The uncertainty of the GPP (in gC m\\^{-2}\\ d\\^{-1}\\).

## Source

Pastorello, G., Trotta, C., Canfora, E. et al. The FLUXNET2015 dataset
and the ONEFlux processing pipeline for eddy covariance data. Sci Data
7, 225 (2020). https://doi.org/10.1038/s41597-020-0534-3

## Examples

``` r
require(ggplot2); require(tidyr)
#> Loading required package: ggplot2
#> Loading required package: tidyr
p_model_validation %>% tidyr::unnest(data) 
#> # A tibble: 2,190 × 4
#>    sitename date         gpp gpp_unc
#>    <chr>    <date>     <dbl>   <dbl>
#>  1 FR-Pue   2007-01-01 2.21  0.0108 
#>  2 FR-Pue   2007-01-02 2.23  0.00475
#>  3 FR-Pue   2007-01-03 2.48  0.00727
#>  4 FR-Pue   2007-01-04 1.71  0.00516
#>  5 FR-Pue   2007-01-05 2.61  0.00764
#>  6 FR-Pue   2007-01-06 2.85  0.00610
#>  7 FR-Pue   2007-01-07 2.80  0.0164 
#>  8 FR-Pue   2007-01-08 1.90  0.0116 
#>  9 FR-Pue   2007-01-09 3.42  0.0147 
#> 10 FR-Pue   2007-01-10 0.651 0.0151 
#> # ℹ 2,180 more rows
```
