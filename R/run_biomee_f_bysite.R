#' Run BiomeE (R wrapper)
#' 
#' Run BiomeE Fortran model on single site.
#'
#' @param sitename Site name.
#' @param params_siml Simulation parameters.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param site_info Site meta info in a data.frame.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param forcing Forcing data.frame used as input.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param params_tile Tile-level model parameters, into a single row data.frame.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param params_species A data.frame containing species-specific model parameters,
#'   with one species per row. See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param init_cohort A data.frame of initial cohort specifications.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param init_soil A data.frame of initial soil pools.
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param makecheck Flag specifying whether checks are performed to verify model inputs and parameters.
#'
#' @export
#' @useDynLib rsofun
#' 
#' @returns Model output is provided as a list, with elements:
#' \describe{
#'   \item{\code{output_hourly_tile}}{A data.frame containing hourly predictions
#'     .
#'     \describe{
#'       \item{year}{Year of the simulation.}
#'       \item{doy}{Day of the year.}
#'       \item{hour}{Hour of the day.}
#'       \item{rad}{Radiation, in W m\eqn{^{-2}}.}
#'       \item{Tair}{Air temperature, in Kelvin.}
#'       \item{Prcp}{Precipitation, in mm m\eqn{^{-2}}.}
#'       \item{GPP}{Gross primary production (kg C m\eqn{^{-2}} hour\eqn{^{-1}}).}
#'       \item{Resp}{Plant respiration (kg C m\eqn{^{-2}} hour\eqn{^{-1}}).}
#'       \item{Transp}{Transpiration (mm m\eqn{^{-2}}).}
#'       \item{Evap}{Evaporation (mm m\eqn{^{-2}}).}
#'       \item{Runoff}{Water runoff (mm m\eqn{^{-2}}).}
#'       \item{Soilwater}{Soil water content in root zone (kg m\eqn{^{-2}}).}
#'       \item{wcl}{Volumetric soil water content for each layer (vol/vol).}
#'       \item{FLDCAP}{Field capacity (vol/vol).}
#'       \item{WILTPT}{Wilting point (vol/vol).}
#'     }}
#'   \item{\code{output_daily_tile}}{A data.frame with daily outputs at a tile
#'     level.
#'     \describe{
#'       \item{year}{Year of the simulation.}
#'       \item{doy}{Day of the year.}
#'       \item{Tc}{Air temperature (Kelvin).}
#'       \item{Prcp}{Precipitation (mm m\eqn{^{-2}}).}
#'       \item{totWs}{Soil water content in root zone (kg m\eqn{^{-2}}).}
#'       \item{Trsp}{Transpiration (mm m\eqn{^{2-}}).}
#'       \item{Evap}{Evaporation (mm m\eqn{^{-2}}).}
#'       \item{Runoff}{Water runoff (mm m\eqn{^{-2}}).}
#'       \item{ws1}{Volumetric soil water content for layer 1.}
#'       \item{ws2}{Volumetric soil water content for layer 2.}
#'       \item{ws3}{Volumetric soil water content for layer 3.}
#'       \item{LAI}{Leaf area index (m\eqn{^2}/m\eqn{^2}).}
#'       \item{GPP}{Gross primary production (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{Rauto}{Plant autotrophic respiration (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{Rh}{Heterotrophic respiration (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{NSC}{Non-structural carbon (kg C m\eqn{^{-2}}).}
#'       \item{seedC}{Biomass of seeds (kg C m\eqn{^{-2}}).}
#'       \item{leafC}{Biomass of leaves (kg C m\eqn{^{-2}}).}
#'       \item{rootC}{Biomass of fine roots (kg C m\eqn{^{-2}}).}
#'       \item{SW_C}{Biomass of sapwood (kg C m\eqn{^{-2}}).}
#'       \item{HW_C}{biomass of heartwood (kg C m\eqn{^{-2}}).}
#'       \item{NSN}{Non-structural N pool (kg N m\eqn{^{-2}}).}
#'       \item{seedN}{Nitrogen of seeds (kg N m\eqn{^{-2}}).}
#'       \item{leafN}{Nitrogen of leaves (kg N m\eqn{^{-2}}).}
#'       \item{rootN}{Nitrogen of roots (kg N m\eqn{^{-2}}).}
#'       \item{SW_N}{Nitrogen of sapwood (kg N m\eqn{^{-2}}).}
#'       \item{HW_N}{Nitrogen of heartwood (kg N m\eqn{^{-2}}).}
#'       \item{McrbC}{Microbial carbon (kg C m\eqn{^{-2}}).}
#'       \item{fastSOM}{Fast soil carbon pool (kg C m\eqn{^{-2}}).}
#'       \item{slowSOM}{Slow soil carbon pool (kg C m\eqn{^{-2}}).}
#'       \item{McrbN}{Microbial nitrogen (kg N m\eqn{^{-2}}).}
#'       \item{fastSoilN}{Fast soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{slowSoilN}{Slow soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{mineralN}{Mineral nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{N_uptk}{Nitrogen uptake (kg N m\eqn{^{-2}}).}
#'     }}
#'   \item{\code{output_daily_cohorts}}{A data.frame with daily predictions
#'     for each canopy cohort.
#'     \describe{
#'       \item{year}{Year of the simulation.}
#'       \item{doy}{Day of the year.}
#'       \item{hour}{Hour of the day.}
#'       \item{cID}{An integer indicating the cohort identity.}
#'       \item{PFT}{An integer indicating the Plant Functional Type.}
#'       \item{layer}{An integer indicating the crown layer, numbered from top
#'         to bottom.}
#'       \item{density}{Number of trees per area (trees ha\eqn{^{-1}}).}
#'       \item{f_layer}{Fraction of layer area occupied by this cohort.}
#'       \item{LAI}{Leaf area index (m\eqn{^2}/m\eqn{^2}).}
#'       \item{gpp}{Gross primary productivity (kg C tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{resp}{Plant autotrophic respiration (kg C tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{transp}{Transpiration (mm tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{NPPleaf}{Carbon allocated to leaves (kg C tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{NPProot}{Carbon allocated to fine roots (kg C tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{NPPwood}{Carbon allocated to wood (kg C tree\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{NSC}{Nonstructural carbohydrates of a tree in this cohort (kg C 
#'         tree\eqn{^{-1}}).}
#'       \item{seedC}{Seed biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'       \item{leafC}{Leaf biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'       \item{rootC}{Fine root biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'       \item{SW_C}{Sapwood biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'       \item{HW_C}{Heartwood biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'       \item{NSN}{Nonstructural nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'       \item{seedN}{Seed nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'       \item{leafN}{Leaf nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'       \item{rootN}{Fine root nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'       \item{SW_N}{Sapwood nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'       \item{HW_N}{Heartwood nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'     }}
#'   \item{\code{output_annual_tile}}{A data.frame with annual outputs at tile level.
#'   \describe{
#'     \item{year}{Year of the simulation.}
#'     \item{CAI}{Crown area index (m\eqn{^2}/m\eqn{^2}).}
#'     \item{LAI}{Leaf area index (m\eqn{^2}/m\eqn{^2}).}
#'     \item{Density}{Number of trees per area (trees ha\eqn{^{-1}}).}
#'     \item{DBH}{Diameter at tile level (cm).}
#'     \item{Density12}{Tree density for trees with DBH > 12 cm (individuals 
#'       ha\eqn{^{-1}}).}
#'     \item{DBH12}{Diameter at tile level considering trees with DBH > 12 cm
#'       (cm).}
#'     \item{QMD12}{Quadratic mean diameter at tile level considering trees with
#'       DBH > 12 cm (cm).}
#'     \item{NPP}{Net primary productivity (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{GPP}{Gross primary productivity (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Rauto}{Plant autotrophic respiration (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Rh}{Heterotrophic respiration (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{rain}{Annual precipitation (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{SoilWater}{Soil water content in root zone (kg m\eqn{^{-2}}).}
#'     \item{Transp}{Transpiration (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Evap}{Evaporation (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Runoff}{Water runoff (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{plantC}{Plant biomass (kg C m\eqn{^{-2}}).}
#'     \item{soilC}{Soil carbon (kg C m\eqn{^{-2}}).}
#'     \item{plantN}{Plant nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{soilN}{Soil nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{totN}{Total nitrogen in plant and soil (kg N m\eqn{^{-2}}).}
#'     \item{NSC}{Nonstructural carbohydrates (kg C m\eqn{^{-2}}).}
#'     \item{SeedC}{Seed biomass (kg C m\eqn{^{-2}}).}
#'     \item{leafC}{Leaf biomass (kg C m\eqn{^{-2}}).}
#'     \item{rootC}{Fine root biomass (kg C m\eqn{^{-2}}).}
#'     \item{SapwoodC}{Sapwood biomass (kg C m\eqn{^{-2}}).}
#'     \item{WoodC}{Heartwood biomass (kg C m\eqn{^{-2}}).}
#'     \item{NSN}{Nonstructural nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{SeedN}{Seed nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{leafN}{Leaf nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{rootN}{Fine root nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{SapwoodN}{Sapwood nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{WoodN}{Heartwood nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{McrbC}{Microbial carbon (kg C m\eqn{^{-2}}).}
#'     \item{fastSOM}{Fast soil carbon pool (kg C m\eqn{^{-2}}).}
#'     \item{SlowSOM}{Slow soil carbon pool (kg C m\eqn{^{-2}}).}
#'     \item{McrbN}{Microbial nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{fastSoilN}{Fast soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{slowsoilN}{Slow soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{mineralN}{Mineral nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{N_fxed}{Nitrogen fixation (kg N m\eqn{^{-2}}).}
#'     \item{N_uptk}{Nitrogen uptake (kg N m\eqn{^{-2}}).}
#'     \item{N_yrMin}{Annual available nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{N_P25}{Annual nitrogen from plants to soil (kg N m\eqn{^{-2}}).}
#'     \item{N_loss}{Annual nitrogen loss (kg N m\eqn{^{-2}}).}
#'     \item{totseedC}{Total seed carbon (kg C m\eqn{^{-2}}).}
#'     \item{totseedN}{Total seed nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{Seedling_C}{Total carbon from all compartments but seeds 
#'       (kg C m\eqn{^{-2}}).}
#'     \item{Seeling_N}{Total nitrogen from all compartments but seeds
#'       (kg N m\eqn{^{-2}}).}
#'     \item{MaxAge}{Age of the oldest tree in the tile (years).}
#'     \item{MaxVolume}{Maximum volumne of a tree in the tile (m\eqn{^3}).}
#'     \item{MaxDBH}{Maximum DBH of a tree in the tile (m).}
#'     \item{NPPL}{Growth of a tree, including carbon allocated to leaves
#'       (kg C m\eqn{^{-2}} year\eqn{^{-1}}).}
#'     \item{NPPW}{Growth of a tree, including carbon allocated to sapwood
#'       (kg C m\eqn{^{-2}} year\eqn{^{-1}}).}
#'     \item{n_deadtrees}{Number of trees that died (trees m\eqn{^{-2}} year\eqn{^{-1}}).}
#'     \item{c_deadtrees}{Carbon biomass of trees that died (kg C 
#'       m\eqn{^{-2}} year\eqn{^{-1}}).}
#'     \item{m_turnover}{Continuous biomass turnover (kg C m\eqn{^{-2}} year\eqn{^{-1}}).}
#'     \item{c_turnover_time}{Carbon turnover rate, calculated as the ratio
#'       between plant biomass and NPP (year\eqn{^{-1}}).}
#'   }}
#'   \item{\code{output_annual_cohorts}}{A data.frame of annual outputs at the
#'     cohort level.
#'   \describe{
#'     \item{year}{Year of the simulation.}
#'     \item{cID}{An integer indicating the cohort identity.}
#'     \item{PFT}{An integer indicating the Plant Functional Type.}
#'     \item{layer}{An integer indicating the crown layer, numbered from top to 
#'       bottom.}
#'     \item{density}{Number of trees per area (trees ha\eqn{^{-1}}).}
#'     \item{f_layer}{Fraction of layer area occupied by this cohort.}
#'     \item{dDBH}{Diameter growth of a tree in this cohort (cm year\eqn{^{-1}}).}
#'     \item{dbh}{Tree diameter (cm).}
#'     \item{height}{Tree height (m).}
#'     \item{age}{Age of the cohort (years).}
#'     \item{Acrow}{Crown area of a tree in this cohort (m\eqn{^2}).}
#'     \item{wood}{Sum of sapwood and heartwood biomass of a tree in this cohort
#'      (kg C tree\eqn{^{-1}}).}
#'     \item{nsc}{Nonstructural carbohydrates in a tree (kg C tree\eqn{^{-1}}).}
#'     \item{NSN}{Nonstructural nitrogen of a tree (kg N tree\eqn{^{-1}}).}
#'     \item{NPPtr}{Total growth of a tree, including carbon allocated to seeds, 
#'       leaves, fine roots, and sapwood (kg C tree\eqn{^{-1}} year\eqn{^{-1}}).}
#'     \item{seed}{Fraction of carbon allocated to seeds to total growth.}
#'     \item{NPPL}{Fraction of carbon allocated to leaves to total growth.}
#'     \item{NPPR}{Fraction of carbon allocated to fine roots to total growth.}
#'     \item{NPPW}{Fraction of carbon allocated to sapwood to total growth.}
#'     \item{GPP_yr}{Gross primary productivity of a tree (kg C tree\eqn{^{-1}} 
#'       year\eqn{^{-1}}).}
#'     \item{NPP_yr}{Net primary productivity of a tree (kg C tree\eqn{^{-1}} 
#'       year\eqn{^{-1}}).}
#'     \item{Rauto}{Plant autotrophic respiration (kg C tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{N_uptk}{Nitrogen uptake (kg N tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{N_fix}{Nitrogen fixation (kg N tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{maxLAI}{Maximum leaf area index for a tree (m\eqn{^2} m\eqn{^{-2}}).}
#'     \item{Volume}{Tree volume (m\eqn{^3}).}
#'     \item{n_deadtrees}{Number of trees that died (trees yr\eqn{^{-1}}).}
#'     \item{c_deadtrees}{Carbon biomass of trees that died (kg C yr\eqn{^{-1}}).}
#'     \item{deathrate}{Mortality rate of this cohort (yr\eqn{^{-1}}).}
#'   }}
#' }
#' 
#' @examples
#' \donttest{
#' # Example BiomeE model run
#' 
#' # Use example drivers data
#' drivers <- biomee_gs_leuning_drivers
#' 
#' # Run BiomeE for the first site
#' mod_output <- run_biomee_f_bysite(
#'  sitename = drivers$sitename[1],
#'  params_siml = drivers$params_siml[[1]],
#'  site_info = drivers$site_info[[1]],
#'  forcing = drivers$forcing[[1]],
#'  params_tile = drivers$params_tile[[1]],
#'  params_species = drivers$params_species[[1]],
#'  init_cohort = drivers$init_cohort[[1]],
#'  init_soil = drivers$init_soil[[1]]
#' )
#' }

run_biomee_f_bysite <- function(
    sitename,
    params_siml,
    site_info,
    forcing,
    params_tile,
    params_species,
    init_cohort,
    init_soil,
    makecheck = TRUE
){
  
  # predefine variables for CRAN check compliance
  type <- NULL
  
  forcing_features <- c(
    'ppfd',
    'temp',
    'vpd',
    'rain',
    'wind',
    'patm',
    'co2'
  )
  
  # select relevant columns of the forcing data
  forcing <- forcing %>%
    select(
      any_of(forcing_features)
    )
  
  runyears <- ifelse(
    params_siml$spinup,
    (params_siml$spinupyears + params_siml$nyeartrend),
    params_siml$nyeartrend)
  
  n_daily  <- params_siml$nyeartrend * 365
  
  # Types of photosynthesis model
  if (params_siml$method_photosynth == "gs_leuning"){
    code_method_photosynth <- 1
    if (is.null(params_siml$steps_per_day))
      stop(
        "Parameter 'steps_per_day' is required."
      )
  } else if (params_siml$method_photosynth == "pmodel"){
    code_method_photosynth <- 2
    if (is.null(params_siml$steps_per_day))
      params_siml$steps_per_day <- 1
    else if (params_siml$steps_per_day > 1){
      stop(
        "run_biomee_f_bysite: time step must be daily 
         for P-model photosynthesis setup."
      )
    }
  } else {
    stop(
      paste("run_biomee_f_bysite:
            params_siml$method_photosynth not recognised:",
            params_siml$method_photosynth))
  }
  
  # Types of mortality formulations
  if (params_siml$method_mortality == "cstarvation"){
    code_method_mortality <- 1
  } else if (params_siml$method_mortality == "growthrate"){
    code_method_mortality <- 2
  } else if (params_siml$method_mortality == "dbh"){
    code_method_mortality <- 3
  } else if (params_siml$method_mortality == "const_selfthin"){
    code_method_mortality <- 4
  } else if (params_siml$method_mortality == "bal"){
    code_method_mortality <- 5
  } else {
    stop(
      paste("run_biomee_f_bysite: params_siml$method_mortality not recognised:",
            params_siml$method_mortality))
  }
  
  # base state, always execute the call
  continue <- TRUE
  
  # validate input
  if (makecheck){
    # Add input and parameter checks here if applicable.
    data_integrity <- lapply(
      forcing_features,
      function(check_var){
        if (any(is.nanull(forcing[check_var]))){
          warning(
            sprintf("Error: Missing forcing %s for site %s",
                    check_var, sitename))
          return(FALSE)
        } else {
          return(TRUE)
        }
      })
    
    # only return true if all checked variables are TRUE
    # suppress warning on coercion of list to single logical
    continue <- suppressWarnings(all(as.vector(data_integrity)))
  }
  
  if (continue) {
    
    ## C wrapper call
    biomeeout <- .Call(
      
      'biomee_f_C',
      
      ## Simulation parameters
      spinup                = as.logical(params_siml$spinup),
      spinupyears           = as.integer(params_siml$spinupyears),
      recycle               = as.integer(params_siml$recycle),
      firstyeartrend        = as.integer(params_siml$firstyeartrend),
      nyeartrend            = as.integer(params_siml$nyeartrend),
      do_U_shaped_mortality = as.logical(params_siml$do_U_shaped_mortality),
      update_annualLAImax   = as.logical(params_siml$update_annualLAImax),
      do_closedN_run        = as.logical(params_siml$do_closedN_run),
      code_method_photosynth= as.integer(code_method_photosynth),
      code_method_mortality = as.integer(code_method_mortality),
      
      ## site meta info
      longitude             = as.numeric(site_info$lon),
      latitude              = as.numeric(site_info$lat),
      altitude              = as.numeric(site_info$elv),
      
      ## Tile-level parameters
      soiltype     = as.integer(params_tile$soiltype),
      FLDCAP       = as.numeric(params_tile$FLDCAP),
      WILTPT       = as.numeric(params_tile$WILTPT),
      K1           = as.numeric(params_tile$K1),
      K2           = as.numeric(params_tile$K2),
      K_nitrogen   = as.numeric(params_tile$K_nitrogen),
      MLmixRatio   = as.numeric(params_tile$MLmixRatio),
      etaN         = as.numeric(params_tile$etaN),
      LMAmin       = as.numeric(params_tile$LMAmin),
      fsc_fine     = as.numeric(params_tile$fsc_fine),
      fsc_wood     = as.numeric(params_tile$fsc_wood),
      GR_factor    = as.numeric(params_tile$GR_factor),
      l_fract      = as.numeric(params_tile$l_fract),
      retransN     = as.numeric(params_tile$retransN),
      f_initialBSW = as.numeric(params_tile$f_initialBSW),
      f_N_add      = as.numeric(params_tile$f_N_add),
      tf_base      = as.numeric(params_tile$tf_base),
      par_mort     = as.numeric(params_tile$par_mort),
      par_mort_under = as.numeric(params_tile$par_mort_under),
      
      ## Species-specific parameters
      n_params_species = as.integer(nrow(params_species)),
      params_species = as.matrix(params_species),
      
      ## initial cohort sizes
      n_init_cohort = as.integer(nrow(init_cohort)),
      init_cohort = as.matrix(init_cohort),
      
      ## initial soil pools
      init_fast_soil_C = as.numeric(init_soil$init_fast_soil_C),
      init_slow_soil_C = as.numeric(init_soil$init_slow_soil_C),
      init_Nmineral    = as.numeric(init_soil$init_Nmineral),
      N_input          = as.numeric(init_soil$N_input),
      n                = as.integer(nrow(forcing)), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_daily          = as.integer(n_daily), 
      n_annual         = as.integer(runyears), 
      n_annual_cohorts = as.integer(params_siml$nyeartrend), # to get cohort outputs after spinup year
      #n_annual_cohorts = as.integer(runyears), # to get cohort outputs from year 1
      forcing          = as.matrix(forcing),
      steps_per_day    = as.integer(params_siml$steps_per_day)
    )
    
    # If simulation is very long, output gets massive.
    # E.g., In a 3000 years-simulation 'biomeeout' is 11.5 GB.
    # In such cases (here, more than 5 GB), ignore hourly and daily outputs at tile and cohort levels
    size_of_object_gb <- as.numeric(
      gsub(
        pattern = " Gb",
        replacement = "",
        format(
          utils::object.size(biomeeout), 
          units = "GB"
        )
      )
    )
    
    if (size_of_object_gb >= 5){
      warning(
        sprintf("Warning: Excessive size of output object (%s) for %s. 
                Hourly and daily outputs at tile and cohort levels are not returned.",
                format(
                  utils::object.size(biomeeout), 
                  units = "GB"
                ), 
                sitename))
    }
    
    # daily_tile
    if (size_of_object_gb < 5){
      output_daily_tile <- as.data.frame(biomeeout[[1]], stringAsFactor = FALSE)
      colnames(output_daily_tile) <- c(
        "year", 
        "doy", 
        "Tc",
        "Prcp", 
        "totWs", 
        "Trsp",
        "Evap", 
        "Runoff", 
        "ws1",
        "ws2", 
        "ws3", 
        "LAI",
        "GPP", 
        "Rauto", 
        "Rh",
        "NSC", 
        "seedC", 
        "leafC",
        "rootC", 
        "SW_C", 
        "HW_C",
        "NSN", 
        "seedN", 
        "leafN",
        "rootN", 
        "SW_N", 
        "HW_N",
        "McrbC", 
        "fastSOM", 
        "slowSOM",
        "McrbN", 
        "fastSoilN", 
        "slowSoilN",
        "mineralN", 
        "N_uptk")
    } else {
      output_daily_tile <- NA
    }
    
    # annual tile
    output_annual_tile <- as.data.frame(biomeeout[[2]], stringAsFactor = FALSE)
    colnames(output_annual_tile) <- c(
      "year", 
      "CAI", 
      "LAI",
      "Density", 
      "DBH", 
      "Density12",
      "DBH12", 
      "QMD12",
      "NPP",
      "GPP", 
      "Rauto", 
      "Rh",
      "rain", 
      "SoilWater",
      "Transp",
      "Evap", 
      "Runoff", 
      "plantC",
      "soilC", 
      "plantN", 
      "soilN",
      "totN", 
      "NSC", 
      "SeedC", 
      "leafC",
      "rootC", 
      "SapwoodC", 
      "WoodC",
      "NSN", 
      "SeedN", 
      "leafN",
      "rootN", 
      "SapwoodN", 
      "WoodN",
      "McrbC", 
      "fastSOM", 
      "SlowSOM",
      "McrbN", 
      "fastSoilN", 
      "slowSoilN",
      "mineralN", 
      "N_fxed", 
      "N_uptk",
      "N_yrMin", 
      "N_P2S", 
      "N_loss",
      "totseedC", 
      "totseedN", 
      "Seedling_C",
      "Seedling_N", 
      "MaxAge", 
      "MaxVolume",
      "MaxDBH", 
      "NPPL", 
      "NPPW",
      "n_deadtrees", 
      "c_deadtrees", 
      "m_turnover", 
      "c_turnover_time"
    )
    
    #--- annual cohorts ----
    annual_values <- c(
      "year",
      "cID",
      "PFT",
      "layer",
      "density",
      "flayer",
      "DBH",
      "dDBH",
      "height",
      "age",
      "BA",
      "dBA",
      "Acrown",
      "Aleaf",
      "nsc",
      "seedC",
      "leafC",
      "rootC",
      "sapwC",
      "woodC",
      "nsn",
      "treeG",
      "fseed",
      "fleaf",
      "froot",
      "fwood",
      "GPP",
      "NPP",
      "Rauto",
      "Nupt",
      "Nfix",
      "n_deadtrees",
      "c_deadtrees",
      "deathrate"
    )
    output_annual_cohorts <- lapply(1:length(annual_values), function(x){
      loc <- 2 + x
      v <- data.frame(
        as.vector(biomeeout[[loc]]),
        stringsAsFactors = FALSE)
      names(v) <- annual_values[x]
      return(v)
    })
    
    # bind columns
    output_annual_cohorts <- do.call("cbind", output_annual_cohorts)
    cohort <- as.character(1:nrow(output_annual_cohorts))
    output_annual_cohorts <- cbind(cohort,
                                   output_annual_cohorts)
    
    # drop rows (cohorts) with no values
    output_annual_cohorts$year[output_annual_cohorts$year == -9999 | 
                                 output_annual_cohorts$year == 0] <- NA
    output_annual_cohorts <- 
      output_annual_cohorts[!is.na(output_annual_cohorts$year),]
    
    # format the output in a structured list
    out <- list(
      # output_hourly_tile = output_hourly_tile,
      output_daily_tile = output_daily_tile,
      # output_daily_cohorts = output_daily_cohorts,
      output_annual_tile = output_annual_tile,
      output_annual_cohorts = output_annual_cohorts)
    
  } else {
    out <- NA
  }
  
  return(out)
  
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
