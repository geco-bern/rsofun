# Run BiomeE (R wrapper)

Run BiomeE Fortran model on single site.

## Usage

``` r
run_biomee_f_bysite(
  sitename,
  params_siml,
  site_info,
  forcing,
  params_tile,
  params_species,
  init_cohort,
  init_soil,
  init_lu = NULL,
  luc_forcing = NULL,
  makecheck = TRUE
)
```

## Arguments

- sitename:

  Site name.

- params_siml:

  Simulation parameters.

- site_info:

  Site meta info in a data.frame.

- forcing:

  A data.frame of forcing climate data, used as input.

- params_tile:

  Tile-level model parameters, into a single row data.frame.

- params_species:

  A data.frame containing species-specific model parameters, with one
  species per row. See examples
  [`biomee_gs_leuning_drivers`](https://geco-bern.github.io/rsofun/dev/reference/biomee_gs_leuning_drivers.md)
  or
  [`biomee_p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_drivers.md)

- init_cohort:

  A data.frame of initial cohort specifications.

- init_soil:

  A data.frame of initial soil pools.

- init_lu:

  A data.frame of initial land unit (LU) specifications.

- luc_forcing:

  An array of land use change (LUC) used during transient phase.

  For further specifications of above inputs and examples see
  [`biomee_gs_leuning_drivers`](https://geco-bern.github.io/rsofun/dev/reference/biomee_gs_leuning_drivers.md),
  [`biomee_p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_drivers.md),
  or
  [`biomee_p_model_luluc_drivers`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_luluc_drivers.md).

- makecheck:

  A logical specifying whether checks are performed to verify forcings
  and model parameters. `TRUE` by default.

## Value

A data.frame with columns containing model output for each land unit
(LU). See examples
[`biomee_gs_leuning_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_gs_leuning_output.md),
[`biomee_p_model_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_output.md),
or
[`biomee_p_model_luluc_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_luluc_output.md).
If only one land unit (LU) is simulated, the column is named 'data'. If
multiple land units (LU) are simulated, the columns are named according
to the LU names. If multiple land units (LU) are simulated, an
additional column 'aggregated' contains output aggregating all tiles as
well as product pools. Model output for each land unit (LU) is provided
as a list. Each list has elements: `output_daily_tile`,
`output_annual_tile`, and `output_annual_cohorts`. Model output for the
aggregated land units (LU) is provided as a list containing
`output_daily_cell`.

- `output_daily_tile`:

  A data.frame with daily outputs at tile level.

  year

  :   Year of the simulation.

  doy

  :   Day of the year.

  Tk

  :   Air temperature (Kelvin).

  Prcp

  :   Precipitation (mm m\\^{-2}\\ day\\^{-1}\\).

  SoilWater

  :   Soil water content in root zone (kg m\\^{-2}\\).

  Transp

  :   Transpiration (mm m\\^{2-}\\ day\\^{-1}\\).

  Evap

  :   Evaporation (mm m\\^{-2}\\ day\\^{-1}\\).

  Runoff

  :   Water runoff (mm m\\^{-2}\\ day\\^{-1}\\).

  ws1

  :   Volumetric soil water content for layer 1.

  ws2

  :   Volumetric soil water content for layer 2.

  ws3

  :   Volumetric soil water content for layer 3.

  LAI

  :   Leaf area index (m\\^2\\/m\\^2\\).

  NPP

  :   Net primary productivity (kg C m\\^{-2}\\ day\\^{-1}\\).

  GPP

  :   Gross primary production (kg C m\\^{-2}\\ day\\^{-1}\\).

  Rauto

  :   Plant autotrophic respiration (kg C m\\^{-2}\\ day\\^{-1}\\).

  Rh

  :   Heterotrophic respiration (kg C m\\^{-2}\\ day\\^{-1}\\).

  NSC

  :   Non-structural carbon (kg C m\\^{-2}\\).

  seedC

  :   Biomass of seeds (kg C m\\^{-2}\\).

  leafC

  :   Biomass of leaves (kg C m\\^{-2}\\).

  rootC

  :   Biomass of fine roots (kg C m\\^{-2}\\).

  sapwoodC

  :   Biomass of sapwood (kg C m\\^{-2}\\).

  heartwoodC

  :   Biomass of heartwood (kg C m\\^{-2}\\).

  NSN

  :   Non-structural N pool (kg N m\\^{-2}\\).

  seedN

  :   Nitrogen of seeds (kg N m\\^{-2}\\).

  leafN

  :   Nitrogen of leaves (kg N m\\^{-2}\\).

  rootN

  :   Nitrogen of roots (kg N m\\^{-2}\\).

  sapwoodN

  :   Nitrogen of sapwood (kg N m\\^{-2}\\).

  heartwoodN

  :   Nitrogen of heartwood (kg N m\\^{-2}\\).

  mcrbC

  :   Microbial carbon (kg C m\\^{-2}\\).

  fastSOM

  :   Fast soil carbon pool (kg C m\\^{-2}\\).

  slowSOM

  :   Slow soil carbon pool (kg C m\\^{-2}\\).

  mcrbN

  :   Microbial nitrogen (kg N m\\^{-2}\\).

  fastSoilN

  :   Fast soil nitrogen pool (kg N m\\^{-2}\\).

  slowSoilN

  :   Slow soil nitrogen pool (kg N m\\^{-2}\\).

  mineralN

  :   Mineral nitrogen pool (kg N m\\^{-2}\\).

  N_uptk

  :   Nitrogen uptake (kg N m\\^{-2}\\ day\\^{-1}\\).

- `output_annual_tile`:

  A data.frame with annual outputs at tile level.

  year

  :   Year of the simulation.

  CAI

  :   Crown area index (m\\^2\\/m\\^2\\).

  LAI

  :   Leaf area index (m\\^2\\/m\\^2\\).

  Density

  :   Number of trees per area (trees ha\\^{-1}\\).

  DBH

  :   Diameter at tile level (cm).

  Density12

  :   Tree density for trees with DBH \> 12 cm (individuals
      ha\\^{-1}\\).

  DBH12

  :   Diameter at tile level considering trees with DBH \> 12 cm(cm).

  QMD12

  :   Quadratic mean diameter at tile level considering trees withDBH \>
      12 cm (cm).

  NPP

  :   Net primary productivity (kg C m\\^{-2}\\ yr\\^{-1}\\).

  GPP

  :   Gross primary productivity (kg C m\\^{-2}\\ yr\\^{-1}\\).

  Rauto

  :   Plant autotrophic respiration (kg C m\\^{-2}\\ yr\\^{-1}\\).

  Rh

  :   Heterotrophic respiration (kg C m\\^{-2}\\ yr\\^{-1}\\).

  Prcp

  :   Annual precipitation (mm m\\^{-2}\\ yr\\^{-1}\\).

  SoilWater

  :   Soil water content in root zone (kg m\\^{-2}\\).

  Transp

  :   Transpiration (mm m\\^{-2}\\ yr\\^{-1}\\).

  Evap

  :   Evaporation (mm m\\^{-2}\\ yr\\^{-1}\\).

  Runoff

  :   Water runoff (mm m\\^{-2}\\ yr\\^{-1}\\).

  plantC

  :   Plant biomass (kg C m\\^{-2}\\).

  soilC

  :   Soil carbon (kg C m\\^{-2}\\).

  totC

  :   Total carbon in plant and soil (kg C m\\^{-2}\\).

  plantN

  :   Plant nitrogen (kg N m\\^{-2}\\).

  soilN

  :   Soil nitrogen (kg N m\\^{-2}\\).

  totN

  :   Total nitrogen in plant and soil (kg N m\\^{-2}\\).

  NSC

  :   Nonstructural carbohydrates (kg C m\\^{-2}\\).

  seedC

  :   Seed biomass (kg C m\\^{-2}\\).

  leafC

  :   Leaf biomass (kg C m\\^{-2}\\).

  rootC

  :   Fine root biomass (kg C m\\^{-2}\\).

  sapwoodC

  :   Sapwood biomass (kg C m\\^{-2}\\).

  heartwoodC

  :   Heartwood biomass (kg C m\\^{-2}\\).

  NSN

  :   Nonstructural nitrogen (kg N m\\^{-2}\\).

  seedN

  :   Seed nitrogen (kg N m\\^{-2}\\).

  leafN

  :   Leaf nitrogen (kg N m\\^{-2}\\).

  rootN

  :   Fine root nitrogen (kg N m\\^{-2}\\).

  sapwoodN

  :   Sapwood nitrogen (kg N m\\^{-2}\\).

  heartwoodN

  :   Heartwood nitrogen (kg N m\\^{-2}\\).

  mcrbC

  :   Microbial carbon (kg C m\\^{-2}\\).

  fastSOM

  :   Fast soil carbon pool (kg C m\\^{-2}\\).

  slowSOM

  :   Slow soil carbon pool (kg C m\\^{-2}\\).

  mcrbN

  :   Microbial nitrogen (kg N m\\^{-2}\\).

  fastSoilN

  :   Fast soil nitrogen pool (kg N m\\^{-2}\\).

  slowsoilN

  :   Slow soil nitrogen pool (kg N m\\^{-2}\\).

  mineralN

  :   Mineral nitrogen pool (kg N m\\^{-2}\\).

  N_fxed

  :   Nitrogen fixation (kg N m\\^{-2}\\).

  N_uptk

  :   Nitrogen uptake (kg N m\\^{-2}\\).

  N_yrMin

  :   Annual available nitrogen (kg N m\\^{-2}\\).

  N_P2S

  :   Annual nitrogen from plants to soil (kg N m\\^{-2}\\).

  N_loss

  :   Annual nitrogen loss (kg N m\\^{-2}\\).

  totseedC

  :   Total seed carbon (kg C m\\^{-2}\\).

  totseedN

  :   Total seed nitrogen (kg N m\\^{-2}\\).

  Seedling_C

  :   Total carbon from all compartments but seeds (kg C m\\^{-2}\\).

  Seedling_N

  :   Total nitrogen from all compartments but seeds(kg N m\\^{-2}\\).

  MaxAge

  :   Age of the oldest tree in the tile (years).

  MaxVolume

  :   Maximum volume of a tree in the tile (m\\^3\\).

  MaxDBH

  :   Maximum DBH of a tree in the tile (m).

  NPPL

  :   Growth of a tree, including carbon allocated to leaves(kg C
      m\\^{-2}\\ yr\\^{-1}\\).

  NPPW

  :   Growth of a tree, including carbon allocated to sapwood(kg C
      m\\^{-2}\\ yr\\^{-1}\\).

  n_deadtrees

  :   Number of trees that died (trees m\\^{-2}\\ yr\\^{-1}\\).

  c_deadtrees

  :   Carbon biomass of trees that died (kg C m\\^{-2}\\ yr\\^{-1}\\).

  m_turnover

  :   Continuous biomass turnover (kg C m\\^{-2}\\ yr\\^{-1}\\).

  c_turnover_time

  :   Carbon turnover rate, calculated as the ratio between plant
      biomass and NPP (yr\\^{-1}\\).

  lu_fraction

  :   Fraction of BiomeE grid cell that is occupied by this land unit
      (LU tile) tile (unitless, or m\\^{-2}\\ LU area per m\\^{-2}\\
      grid cell area).

- `output_annual_cohorts`:

  A data.frame of annual outputs at the cohort level.

  year

  :   Year of the simulation.

  cID

  :   An integer indicating the cohort identity.

  PFT

  :   An integer indicating the Plant Functional Type.

  layer

  :   An integer indicating the crown layer, numbered from top to
      bottom.

  density

  :   Number of trees per area (trees ha\\^{-1}\\).

  flayer

  :   Fraction of layer area occupied by this cohort.

  DBH

  :   Tree diameter (cm).

  dDBH

  :   Diameter growth of a tree in this cohort (cm yr\\^{-1}\\).

  height

  :   Tree height (m).

  age

  :   Age of the cohort (years).

  BA

  :   Basal area a tree in this cohort (m\\^2\\ tree\\^{-1}\\).

  dBA

  :   Basal area increment of a tree in this cohort (m\\^2\\
      tree\\^{-1}\\ yr\\^{-1}\\).

  Acrown

  :   Crown area of a tree in this cohort (m\\^2\\ tree\\^{-1}\\).

  Aleaf

  :   Total area of leaves (m\\^2\\ tree\\^{-1}\\).

  wood

  :   Sum of sapwood and heartwood biomass of a tree in this cohort (kg
      C tree\\^{-1}\\).

  NSC

  :   Non-structural carbon of a tree in this cohort (kg C
      tree\\^{-1}\\).

  seedC

  :   Biomass of seeds of a tree in this cohort (kg C tree\\^{-1}\\).

  leafC

  :   Biomass of leaves of a tree in this cohort (kg C tree\\^{-1}\\).

  rootC

  :   Biomass of fine roots of a tree in this cohort (kg C
      tree\\^{-1}\\).

  sapwoodC

  :   Biomass of sapwood of a tree in this cohort (kg C tree\\^{-1}\\).

  heartwoodC

  :   Biomass of heartwood of a tree in this cohort (kg C
      tree\\^{-1}\\).

  NSN

  :   Non-structural nitrogen of a tree in this cohort (kg N
      tree\\^{-1}\\).

  treeG

  :   Total growth of a tree, including carbon allocated to seeds,
      leaves, fine roots, and sapwood (kg C tree\\^{-1}\\ yr\\^{-1}\\).

  fseed

  :   Fraction of carbon allocated to seeds to total growth.

  fleaf

  :   Fraction of carbon allocated to leaves to total growth.

  froot

  :   Fraction of carbon allocated to fine roots to total growth.

  fwood

  :   Fraction of carbon allocated to sapwood to total growth.

  NPP

  :   Net primary productivity of a tree (kg C tree\\^{-1}\\
      yr\\^{-1}\\).

  GPP

  :   Gross primary productivity of a tree (kg C tree\\^{-1}\\
      yr\\^{-1}\\).

  Rauto

  :   Plant autotrophic respiration (kg C tree\\^{-1}\\ yr\\^{-1}\\).

  N_uptk

  :   Nitrogen uptake (kg N tree\\^{-1}\\ yr\\^{-1}\\).

  N_fxed

  :   Nitrogen fixation (kg N tree\\^{-1}\\ yr\\^{-1}\\).

  deathrate

  :   Mortality rate of this cohort, including natural mortality,
      starvation and any other processes causing a loss of individuals
      in general (yr\\^{-1}\\).

  n_deadtrees

  :   Plant to soil N flux due to mortality, including natural
      mortality, starvation and any other processes causing a loss of
      individuals in general (kg N yr\\^{-1}\\ m\\^{-2}\\).

  c_deadtrees

  :   Plant to soil C flux due to mortality, including natural
      mortality, starvation and any other processes causing a loss of
      individuals in general (kg C yr\\^{-1}\\ m\\^{-2}\\).

If there are multiple land units (LU) there will also be a column named
\`aggregated\` containing a data.frame in the column
\`output_annual_cell\` with annual outputs aggregating all tiles present
in the simulation cell. Note that quantities per m2 refer to m2 of grid
cell area, i.e. the full area of the BiomeE simulation. 'lu_fraction'
refers to the sum of all the tiles, which must remain constant and which
represents the fraction of the cell area that is not water/ice. In most
cases, it would be close to 1. It contains columns:

- `output_annual_cell`:

  A data.frame with annual outputs aggregating all tiles present in the
  simulation cell. Note that quantities per m\\^{2}\\ refer to m\\^{2}\\
  of grid cell area, i.e. the full area of the BiomeE simulation.
  'lu_fraction' refers to the sum of all the tiles, which must remain
  constant and which represents the fraction of the cell area that is
  not water/ice. In most cases, it would be close to 1.

  all columns from 'output_yearly_tile'

  :   See above for output_yearly_tile, but now expressed per unit area
      of the BiomeE grid cell.

  lu_fraction

  :   Fraction of BiomeE grid cell that is occupied by this land unit
      (LU tile) tile (unitless, or m\\^{2}\\ LU area per m\\^{2}\\ grid
      cell area).

  prod_pool_1_C

  :   Carbon in product pool 1 (kg C m\\^{-2}\\ grid cell).

  prod_pool_1_N

  :   Nitrogen in product pool 1 (kg N m\\^{-2}\\ grid cell).

  prod_pool_2_C

  :   Carbon in product pool 2 (kg C m\\^{-2}\\ grid cell).

  prod_pool_2_N

  :   Nitrogen in product pool 2 (kg N m\\^{-2}\\ grid cell).

  Rprod_0_C

  :   Carbon loss rate directly from land use change (LUC) (kg C
      m\\^{-2}\\ grid cell yr\\^{-1}\\).

  Rprod_0_N

  :   Nitrogen loss rate directly from land use change (LUC) (kg C
      m\\^{-2}\\ grid cell yr\\^{-1}\\).

  Rprod_1_C

  :   Carbon loss rate from product pool 1 (kg C m\\^{-2}\\ grid cell
      yr\\^{-1}\\).

  Rprod_1_N

  :   Nitrogen loss rate from product pool 1 (kg N m\\^{-2}\\ grid cell
      yr\\^{-1}\\).

  Rprod_2_C

  :   Carbon loss rate from product pool 2 (kg C m\\^{-2}\\ grid cell
      yr\\^{-1}\\).

  Rprod_2_N

  :   Nitrogen loss rate from product pool 2 (kg N m\\^{-2}\\ grid cell
      yr\\^{-1}\\).

## Examples

``` r
 # do not run long-running simulations
# Example BiomeE model run

# Use example drivers data
drivers <- biomee_p_model_drivers

# Run BiomeE for the first site
mod_output <- run_biomee_f_bysite(
 sitename = drivers$sitename[1],
 params_siml = drivers$params_siml[[1]],
 site_info = drivers$site_info[[1]],
 forcing = drivers$forcing[[1]],
 params_tile = drivers$params_tile[[1]],
 params_species = drivers$params_species[[1]],
 init_cohort = drivers$init_cohort[[1]],
 init_soil = drivers$init_soil[[1]]
)
```
