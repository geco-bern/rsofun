# rsofun P-model Vcmax25 validation data

Small example dataset of target observations (leaf trait data) to
optimize model parameters with the function
[`calib_sofun`](https://geco-bern.github.io/rsofun/dev/reference/calib_sofun.md)

## Usage

``` r
p_model_validation_vcmax25
```

## Format

A tibble of validation data:

- sitename:

  A character string containing the site names (e.g.
  'Reichetal_Colorado').

- data:

  A tibble \[ 1 x 2 \] with observations for the following variables:

  vcmax25

  :   The observed maximum rate of carboxylation (Vcmax), normalised to
      25\\^o\\ C (in mol C m\\^{-2}\\ d\\^{-1}\\), aggregated over
      different plant species in each site.

  vcmax25_unc

  :   The uncertainty of the Vcmax25 (in mol C m\\^{-2}\\ d\\^{-1}\\),
      calculated as the standard deviation among Vcmax25 observations
      for several species per site or as the total standard deviation
      across sites for single-plant-species sites.

## Source

Atkin, O. K., Bloomfield, K. J., Reich, P. B., Tjoelker, M. G., Asner,
G. P., Bonal, D., et al. (2015). Global variability in leaf respiration
in relation to climate, plant functional types and leaf traits. New
Phytol. 206 (2), 614–636. doi:10.1111/nph.13253

## Examples

``` r
require(ggplot2); require(tidyr)
p_model_validation_vcmax25 %>% tidyr::unnest(data) 
#> # A tibble: 4 × 3
#> # Groups:   sitename [4]
#>   sitename               vcmax25 vcmax25_unc
#>   <chr>                    <dbl>       <dbl>
#> 1 Reichetal_Colorado   0.0000339   0.0000136
#> 2 Reichetal_New_Mexico 0.0000757   0.0000163
#> 3 Reichetal_Venezuela  0.0000472   0.0000164
#> 4 Reichetal_Wisconsin  0.0000502   0.0000147
```
