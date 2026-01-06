# Run BiomeE

Runs BiomeE model for multiple sites.

## Usage

``` r
runread_biomee_f(drivers, makecheck = TRUE, parallel = FALSE, ncores = 1)
```

## Arguments

- drivers:

  A nested data frame with one row for each site and columns named
  according to the arguments of function
  [`run_biomee_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_biomee_f_bysite.md).
  Namely
  `sitename, params_siml, site_info, forcing, params_tile, params_species, init_cohort`
  and `init_soil`.

- makecheck:

  A logical specifying whether checks are performed to verify forcings
  and model parameters. `TRUE` by default.

- parallel:

  Deprecated. Use ncores instead.

- ncores:

  An integer specifying the number of cores used for parallel computing
  (sites processed in parallel). Default: 1 (no parallel execution).

## Value

A data frame (tibble) with one row for each site. The columns are the
site information `site_info` and one column per land unit (LU) in
addition to an aggregated output `aggregated`. By default, the only LU
is named `data` and `aggregated` is not present since aggregating one LU
is not useful. When multiple LU are configured (using `init_lu`), the
columns are named using the LU name provided in `init_lu`. See
[`run_biomee_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_biomee_f_bysite.md)
for a detailed description of the outputs. Example outputs are provided
as
[`biomee_p_model_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_output.md)
and
[`biomee_p_model_luluc_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_luluc_output.md).

## Examples

``` r
# Example BiomeE model run
 # do not run long-running simulations
runread_biomee_f(
  drivers = biomee_p_model_drivers
)
#> # A tibble: 1 × 2
#>   sitename data            
#>   <chr>    <list>          
#> 1 CH-Lae   <named list [3]>
if (FALSE)  # do not run this long-running example at all, only *show* example
runread_biomee_f(
  drivers = biomee_gs_leuning_drivers
)
 # \dontrun{}
```
