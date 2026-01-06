# rsofun BiomeE driver data (Leuning photosynthesis model)

Example driver to run the BiomeE-model at the CH-LAE site using the
Leuning photosynthesis specification (and half-hourly time step) It can
also be used together with leaf trait data from CH-LAE
([`biomee_validation`](https://geco-bern.github.io/rsofun/dev/reference/biomee_validation.md))
to optimize model parameters.

## Usage

``` r
biomee_gs_leuning_drivers
```

## Format

A tibble of driver data.

- sitename:

  Site name

- params_siml:

  Simulation parameters as a data.frame, including the following data:

  spinup

  :   Flag indicating whether this simulation does spin-up (deprecated).

  spinupyears

  :   Number of spin-up years. Set to 0 for no spinup.

  recycle

  :   Number of first N years of forcing data.frame that are recycled
      for spin-up.

  firstyeartrend

  :   Year of first transient year (AD) (optional). Is only used to set
      years in output data frames. Defaults to 0 if not provided.

  nyeartrend

  :   Number of transient years (optional). Determines the length of
      simulation output after spin-up. Defaults to number of years
      contained in the forcing data. (If longer than forcing data, last
      year of forcing is repeated until the end (spin-down).)

  steps_per_day

  :   Time resolution of the forcing (day-1).

  do_U_shaped_mortality

  :   Flag indicating whether U-shaped mortality is used.

  do_closedN_run

  :   Flag indicating whether doing N closed runs to recover N balance
      enforcing 0.2 kg N m-2 in the inorganic N pool.

  method_photosynth

  :   String specifying the method of photosynthesis used in the model,
      either "pmodel" or "gs_leuning".document()

  method_mortality

  :   String indicating the type of mortality in the model. One of the
      following: "dbh" is size-dependent mortality, "const_selfthin" is
      constant self thinning (in development), "cstarvation" is carbon
      starvation, and "growthrate" is growth rate dependent mortality.

  do_daily_diagnostics

  :   Whether to output daily diagnostics ('output_daily_tile').
      Default: True.

- site_info:

  Site meta info in a data.frame. This data structure can be freely used
  for documenting the dataset, but must include at least the following
  data:

  lon

  :   Longitude of the site location in degrees east.

  lat

  :   Latitude of the site location in degrees north.

  elv

  :   Elevation of the site location, in meters above sea level.

- forcing:

  Forcing data.frame used as input

  ppfd

  :   Photosynthetic photon flux density (mol m-2 s-1)

  tair

  :   Air temperature (deg C)

  vpd

  :   Vapor pressure deficit (Pa)

  rain

  :   Precipitation (kgH2O m-2 s-1 == mm s-1)

  wind

  :   Wind velocity (m s-1)

  pair

  :   Atmospheric pressure (Pa)

  co2

  :   Atmospheric CO\\\_2\\ concentration in ppm.

- params_tile:

  Tile-level model parameters, into a single row data.frame, including
  the following data:

  soiltype

  :   Integer indicating the type of soil: Sand = 1, LoamySand = 2,
      SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7.

  FLDCAP

  :   Field capacity (vol/vol). Water remaining in a soil after it has
      been thoroughly saturated and allowed to drain freely.

  WILTPT

  :   Wilting point (vol/vol). Water content of a soil at which plants
      wilt and fail to recover.

  K1

  :   Fast soil C decomposition rate (yr\\^{-1}\\).

  K2

  :   Slow soil C decomposition rate (yr\\^{-1}\\).

  K_nitrogen

  :   Mineral Nitrogen turnover rate (yr\\^{-1}\\).

  MLmixRatio

  :   Ratio of C and N returned to litters from microbes.

  etaN

  :   N loss rate through runoff (organic and mineral) (yr\\^{-1}\\).

  LMAmin

  :   Minimum LMA, leaf mass per unit area, kg C m\\^{-2}\\.

  fsc_fine

  :   Fraction of fast turnover carbon in fine biomass.

  fsc_wood

  :   Fraction of fast turnover carbon in wood biomass.

  GR_factor

  :   Growth respiration factor.

  l_fract

  :   Fraction of the carbon retained after leaf drop.

  retransN

  :   Retranslocation coefficient of nitrogen.

  f_initialBSW

  :   Coefficient for setting up initial sapwood.

  f_N_add

  :   Re-fill of N for sapwood.

  tf_base

  :   Calibratable scalar for respiration, used to increase LUE levels.

  par_mort

  :   Canopy mortality parameter.

  par_mort_under

  :   Parameter for understory mortality.

- params_species:

  A data.frame containing species-specific model parameters, with one
  species per row, including the following data:

  The following columns pertaining to the **plant type**:

  :   

      lifeform

      :   Integer set to 0 for grasses and 1 for trees.

      phenotype

      :   Integer set to 0 for deciduous and 1 for evergreen.

      pt

      :   Integer indicating the type of plant according to
          photosynthesis: 0 for C3; 1 for C4

  The following columns pertaining to the **root parameters**:

  :   

      alpha_FR

      :   Fine root turnover rate (yr\\^{-1}\\).

      rho_FR

      :   Material density of fine roots (kg C m\\^{-3}\\).

      root_r

      :   Radius of the fine roots, in m.

      root_zeta

      :   e-folding parameter of root vertical distribution, in m.

      Kw_root

      :   Fine root water conductivity (mol m\\^{-2}\\ s\\^{-1}\\
          MPa\\^{-1}\\).

      leaf_size

      :   Characteristic leaf size.

  The following columns pertaining to the **photosynthesis parameters**:

  :   

      Vmax

      :   Max RuBisCo rate, in mol m\\^{-2}\\ s\\^{-1}\\.

      Vannual

      :   Annual productivity per unit area at full sun (kg C m\\^{-2}\\
          year\\^{-2}\\).

      wet_leaf_dreg

      :   Wet leaf photosynthesis down-regulation.

      m_cond

      :   Factor of stomatal conductance.

      alpha_phot

      :   Photosynthesis efficiency.

      gamma_L

      :   Leaf respiration coefficient, in yr\\^{-1}\\.

      gamma_LN

      :   Leaf respiration coefficient per unit N.

      gamma_SW

      :   Sapwood respiration rate, in kg C m\\^{-2}\\ yr\\^{-1}\\.

      gamma_FR

      :   Fine root respiration rate, kg C kg C\\^{-1}\\ yr\\^{-1}\\.

      tk_crit

      :   Critical temperature triggerng offset of phenology, in Kelvin.

      tk_crit_on

      :   Critical temperature triggerng onset of phenology, in Kelvin.

      gdd_crit

      :   Critical value of GDD5 for turning ON growth season.

      betaON

      :   Critical soil moisture for phenology onset.

      betaOFF

      :   Critical soil moisture for phenology offset.

  The following columns pertaining to the **allometry parameters**:

  :   

      alphaHT

      :   Coefficient for allometry (height = alphaHT \* DBH_m \*\*
          thetaHT), in m m\\^{-thetaHT}\\.

      thetaHT

      :   Coefficient for allometry (height = alphaHT \* DBH_m \*\*
          thetaHT), in m m\\^{-thetaHT}\\.

      alphaCA

      :   Coefficient for allometry (projected crown area = pi \*
          (alphaCA \* DBH_m) \*\* thetaCA), in m\\^{2/thetaCA-1}\\.

      thetaCA

      :   Coefficient for allometry (projected crown area = pi \*
          (alphaCA \* DBH_m) \*\* thetaCA), unitless. Dybzinski (eq. G1)
          showed that thetaCA = theatBM - 1.

      alphaBM

      :   Coefficient for allometry (biomass = alphaBM \* DBH \*\*
          thetaBM), in kg C m\\^{-thetaBM}\\.

      thetaBM

      :   Coefficient for allometry (biomass = alphaBM \* DBH \*\*
          thetaBM), unitless. Dybzinski (eq. G1) showed that thetaCA =
          theatBM - 1.

  The following columns pertaining to the **reproduction parameters**:

  :   

      seedlingsize

      :   Initial size of seedlings, in kg C per individual.

      maturalage

      :   Age at which trees can reproduce (years).

      v_seed

      :   Fraction of G_SF to G_F.

  The following columns pertaining to the **mortality parameters**:

  :   

      mortrate_d_c

      :   Canopy tree mortality rate (yr\\^{-1}\\).

      mortrate_d_u

      :   Understory tree mortality rate (yr\\^{-1}\\).

  The following columns pertaining to the **leaf parameters**:

  :   

      LMA

      :   Leaf mass per unit area (kg C m\\^{-2}\\).

      leafLS

      :   TODO

      LNbase

      :   Basal leaf N per unit area, in kg N m\\^{-2}\\.

      CNleafsupport

      :   TODO

      rho_wood

      :   Wood density (kg C m\\^{-3}\\).

      taperfactor

      :   TODO

      lAImax

      :   Maximum crown LAI (leaf area index).

      tauNSC

      :   TODO

      fNSmax

      :   Multiplier for NSNmax as sum of potential bl and br.

      phiCSA

      :   Ratio of sapwood area to leaf area.

  The following columns pertaining to the **C/N ratios for plant pools**:

  :   

      CNleaf0

      :   TODO

      CNsw0

      :   TODO

      CNwood0

      :   TODO

      CNroot0

      :   TODO

      CNseed0

      :   TODO

      Nfixrate0

      :   Reference N fixation rate (kg N kg C\\^{-1}\\ root).

      NfixCost0

      :   Carbon cost of N fixation (kg C kg N\\^{-1}\\).

      internal_gap_frac

      :   TODO

  The following columns pertaining to the **calibratable parameters**:

  :   

      kphio

      :   Quantum yield efficiency \\\varphi_0\\, in mol mol\\^{-1}\\.

      phiRL

      :   Ratio of fine root to leaf area.

      LAI_light

      :   Maximum LAI limited by light.

- init_cohort:

  A data.frame of initial cohort specifications, including the following
  data:

  init_cohort_species

  :   Index of a species described in param_species.

  init_cohort_nindivs

  :   Initial individual density, in individuals per m\\^{2}\\.

  init_cohort_bl

  :   Initial biomass of leaf, in kg C per individual.

  init_cohort_br

  :   Initial biomass of fine root, in kg C per individual.

  init_cohort_bsw

  :   Initial biomass of sapwood, in kg C per individual.

  init_cohort_bHW

  :   Initial biomass of heartwood, in kg C per individual.

  init_cohort_seedC

  :   Initial biomass of seed, in kg C per individual.

  init_cohort_nsc

  :   Initial non-structural biomass, in kg C per individual.

  lu_index

  :   Land use type this cohorts belongs to (given as index in init_lu
      aray). Default: 0 (attach to all LU types except thoses which do
      not accept vegetation – cf init_lu.vegetated).

- init_soil:

  A data.frame of initial soil pools, including the following data:

  init_fast_soil_C

  :   Initial fast soil carbon, in kg C m\\^{-2}\\.

  init_slow_soil_C

  :   Initial slow soil carbon, in kg C m\\^{-2}\\.

  init_Nmineral

  :   Mineral nitrogen pool, in kg N m\\^{-2}\\.

  N_input

  :   Annual nitrogen input to soil N pool, in kg N m\\^{-2}\\
      yr\\^{-1}\\.

- init_lu:

  A data.frame of initial land unit (LU) specifications, including the
  following data:

  fraction

  :   Initial grid cell fraction occupied by this LU, dimensionless (0
      to 1) or m\\^{-2}\\ LU area per m\\^{-2}\\ grid cell area. The sum
      of all fractions is typically equal to 1, but may be less in which
      case the difference is the fraction of the grid cell occupied by
      ice/water.

  preset

  :   Predefined land use type (optional). One of: 'unmanaged', 'urban',
      'cropland', 'pasture'. See below for meaning of these presets.
      Leave empty to not use any preset.

  vegetated

  :   Whether this LU accepts vegetation. Default for preset 'urban':
      False, default for other presets: True.

  extra_N_input

  :   Additional inorg N supply (to account for N fertiliser
      application), in kg m-2 yr-1. Default for preset 'cropland': 0.01,
      default other presets: 0.

  extra_turnover_rate

  :   Additional soil turnover rate (to account for soil management such
      as tillage), dimensionless. Default for preset 'cropland': 0.2,
      default for other presets: 0.

  oxidized_litter_fraction

  :   Fraction of above-ground turnover that is directly oxidized (crop
      and grass harvest), dimensionless. Default for preset 'cropland':
      0.9, default for preset 'pasture': 0.4, default for other presets:
      0.

- luc_forcing:

  Array of land use change (LUC) used during transient phase. During
  spinup, the initial land unit fractions are used (i.e. no transition).
  If there are more transient years than provided LUC data, the last
  state is maintained until the end of the transient phase (i.e. no
  transition). The array is a nxn square matrix, where n is the number
  of LU (i.e. dimension of init_lu). Each entry f(i, j) expresses the
  grid cell fraction of LU i (row) being transfered to LU j (column).
  I.e. same units as `init_lu$fraction`. Self transitions are allowed,
  meaning that a part of the land unit is clear cut, but the area
  remains in the same land use.
