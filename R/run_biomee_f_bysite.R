#' Run BiomeE (R wrapper)
#' 
#' Run BiomeE Fortran model on single site.
#'
#' @param sitename Site name.
#' @param params_siml Simulation parameters.
#' @param site_info Site meta info in a data.frame.
#' @param forcing A data.frame of forcing climate data, used as input.
#' @param params_tile Tile-level model parameters, into a single row data.frame.
#' @param params_species A data.frame containing species-specific model parameters,
#'   with one species per row. See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param init_cohort A data.frame of initial cohort specifications.
#' @param init_soil A data.frame of initial soil pools.
#' @param makecheck A logical specifying whether checks are performed to verify forcings and model parameters. \code{TRUE} by default.
#' @param init_lu A data.frame of initial land unit (LU) specifications.
#' @param luc_forcing An array of land use change (LUC) used during transient phase. Must not be provided if \code{luh2} is.
#' @param luh2 A data.frame containing parameters for parsing LUH2 data instead of \code{lu_forcing} and set up \code{init_lu} (if not provided).
#'
#' For further specifications of above inputs and examples see \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}.
#' 
#' @returns Model output is provided as a list, with elements:
#' \describe{
#'   \item{\code{output_daily_tile}}{A data.frame with daily outputs at a tile level.
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
#'     \item{n_deadtrees}{Plant to soil N flux due to mortality, including natural mortality, starvation and any other rprocesse causing a loss of individuals in general  (kg N yr\eqn{^{-1}} m\eqn{^{-2}}).}
#'     \item{c_deadtrees}{Plant to soil C flux due to mortality, including natural mortality, starvation and any other rprocesse causing a loss of individuals in general  (kg C yr\eqn{^{-1}} m\eqn{^{-2}}).}
#'     \item{deathrate}{Mortality rate of this cohort, including natural mortality, starvation and any other rprocesse causing a loss of individuals in general (yr\eqn{^{-1}}).}
#'   }}
#' }
#'
#' @export
#' @useDynLib rsofun
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
  init_lu = NULL,
  luc_forcing = NULL,
  luh2 = NULL,
  makecheck = TRUE
){
  ndayyear <- 365
  # record number of years in forcing data
  # frame to use as default values (unless provided othrwise as params_siml$nyeartrend)
  forcing_years <- nrow(forcing)/(ndayyear * params_siml$steps_per_day)

  # Add default parameters (backward compatibility layer)
  params_siml <- build_params_siml(params_siml, forcing_years, makecheck)

  # Build LULUC parameters
  if (is.null(luh2)) {
    init_lu     <- build_init_lu(init_lu)
    luc_forcing <- build_luc_forcing(luc_forcing, nrow(init_lu))
  } else {
    print('Parsing LUH2 data...')
    simplified <- ifelse(is.null(luh2$simplified), FALSE, luh2$simplified)
    parsed_luh2 <- parse_luh2(
                                luh2$state_file,
                                luh2$trans_file,
                                site_info$lon,
                                site_info$lat,
                                ifelse(is.null(luh2$start), 1, luh2$start),
                                ifelse(is.null(luh2$n), params_siml$nyeartrend, luh2$n),
                                simplified
    )
    print('Done.')

    if (is.null(init_lu)) {
      # If init_lu is provided, we use these
      if (simplified)
        presets <- c(rep('unmanaged', 2), 'urban', 'cropland', 'pasture')
      else
        presets <- c(rep('unmanaged', 4), 'urban', rep('cropland', 5), rep('pasture', 2))
      init_lu <- tibble(
        name      = names(parsed_luh2$states_init),
        fraction  = parsed_luh2$states_init,
        preset    = presets
      )
      init_lu     <- build_init_lu(init_lu)
    } else {
      # If not, we use default LUH2 ones
      init_lu     <- build_init_lu(init_lu)
    }
    luc_forcing <- build_luc_forcing(parsed_luh2$luc_matrix, nrow(init_lu))
  }

  if (length(luc_forcing[1,1,]) > params_siml$nyeartrend)
    warning(paste("Warning: 'luc_forcing' contains more data points than nyeartrend (", length(luc_forcing[1,1,]), ' vs ', params_siml$nyeartrend, '). Extra data points will be ignored.'))

  # Set up variables used by C wrapper to build output arrays
  n_daily  <- ifelse(params_siml$daily_diagnostics, params_siml$nyeartrend * ndayyear, 0)
  n_annual <- ifelse(
    params_siml$spinup,
    (params_siml$spinupyears + params_siml$nyeartrend),
    params_siml$nyeartrend
  )
  n_annual_trans <- params_siml$nyeartrend

  ## C wrapper call (using prepped data)
  biomeeout <- .Call(
    'biomee_f_C',
    params_siml      = as.matrix(prepare_params_siml(params_siml)),
    site_info        = as.matrix(prepare_site_info(site_info)),
    params_tile      = as.matrix(prepare_params_tile(params_tile)),
    params_species   = as.matrix(prepare_params_species(params_species)),
    init_cohort      = as.matrix(prepare_init_cohorts(init_cohort)),
    init_soil        = as.matrix(prepare_init_soil(init_soil)),
    forcing          = as.matrix(prepare_forcing(forcing)),
    init_lu          = as.matrix(prepare_init_lu(init_lu)),
    luc_forcing      = as.array(prepare_luc_forcing(luc_forcing)),
    n_daily          = as.integer(n_daily),
    n_annual         = as.integer(n_annual),
    n_annual_trans   = as.integer(n_annual_trans)
  )

  out <- build_out(biomeeout, init_lu$name, sitename, params_siml$daily_diagnostics)

  return(out)
}

# Build R output
build_out <- function(biomeeout, lu_names, sitename, daily_diagnostics){
  if (daily_diagnostics) {
    # If simulation is very long, output gets massive.
    # E.g., In a 3000 years-simulation 'biomeeout' is 11.5 GB.
    # In such cases (here, more than 5 GB), drop daily outputs
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

    trimmed_object <- size_of_object_gb >= 5

    if (trimmed_object){
      warning(
        sprintf("Warning: Excessive size of output object (%s) for %s.
                Daily outputs are not returned.",
                format(
                  utils::object.size(biomeeout),
                  units = "GB"
                ),
                sitename))
    }
  } else {
    trimmed_object <- TRUE
  }

  n_lu <- length(lu_names)

  if (n_lu == 1) {
    out <- list("data"=build_lu_out(biomeeout, 1, trimmed_object))
  }
  else {
    out <- list(aggregated_annual_tile_output(biomeeout[[4]]))
    for (lu in 1:n_lu) {
      res <- build_lu_out(biomeeout, lu, trimmed_object)
      out <- append(out, list(res))
    }
    names(out) <- c("aggregated", lu_names)
  }

  return(out)
}

build_lu_out <- function(biomeeout, lu, trimmed_object){
  # Note: drop=FALSE is important to prevent R from dropping dimensions when length is 1.

  # daily_tile
  if (!trimmed_object){
    output_daily_tile <- daily_tile_output(biomeeout[[1]][,,lu,drop=FALSE])
  } else {
    output_daily_tile <- NA
  }

  # annual tile
  output_annual_tile <- annual_tile_output(biomeeout[[2]][,,lu,drop=FALSE])

  # annual cohorts
  output_annual_cohorts <- annual_cohort_output(biomeeout[[3]][,,,lu,drop=FALSE])

  # format the output in a structured list
  out_lu <- list(
    output_daily_tile = output_daily_tile,
    output_annual_tile = output_annual_tile,
    output_annual_cohorts = output_annual_cohorts
  )

  return(out_lu)
}

###### Build and prepare inputs #######
# build_xxx functions check the parameters/data and add default parameters
# prepare_xxx functions preapre the data in a form that is expected by Fortran (i.e. arrays of numbers).
# The use of 'select()' ensure that the data are sent in the right order (and that no column is missing).
# In particular, we remove columns containing characters (to not encounter issues within Fortran).

build_params_siml <- function(params_siml, forcing_years, makecheck){
  `%nin%` <- Negate(`%in%`)
  if ("spinup" %nin% names(params_siml))
    params_siml$spinup <- params_siml$spinupyears > 0
  else if (params_siml$spinup != (params_siml$spinupyears > 0)) {
    warning("Warning: spinup flag is deprecated. Please set spinupyears to 0 to disable spinup.")
    params_siml$spinup <- (params_siml$spinupyears > 0)
  }

  # Default value for nyeartrend
  if ('nyeartrend' %nin% names(params_siml)) {
    params_siml$nyeartrend <- forcing_years
  }
  # Default value for firstyeartrend
  # If not provided, we anchor to 0, meaning that spinup years are negative and transient years are positive.
  # firstyeartrend is currently not used.
  if ('firstyeartrend' %nin% names(params_siml)) {
    params_siml$firstyeartrend <- 0
  }

  # Types of photosynthesis model
  if (params_siml$method_photosynth == "gs_leuning"){
    params_siml$code_method_photosynth <- 1
    if (is.null(params_siml$steps_per_day))
      stop(
        "Parameter 'steps_per_day' is required."
      )
  } else if (params_siml$method_photosynth == "pmodel"){
    params_siml$code_method_photosynth <- 2
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
  params_siml$code_method_mortality <- switch(params_siml$method_mortality,
                                              "cstarvation"    = 1,
                                              "growthrate"     = 2,
                                              "dbh"            = 3,
                                              "const_selfthin" = 4,
                                              "bal"            = 5
  )
  if (is.null(params_siml$code_method_mortality)) {
    stop(
      paste("run_biomee_f_bysite: params_siml$method_mortality not recognised:",
            params_siml$method_mortality))
  }
  if (makecheck){

    if (params_siml$nyeartrend < forcing_years) {
      warning(sprintf(
        "Info: provided value of nyeartrend is less than the number of years of forcing data (%i). Only the first %i will be used."
        , forcing_years, params_siml$nyeartrend))
    }
    if (params_siml$nyeartrend > forcing_years) {
      warning(sprintf(
        "Info: provided value of nyeartrend is greater than the number of years of forcing data (%i). The final year will be repeated as much as needed."
        , forcing_years))
    }
  }

  if ('daily_diagnostics' %nin% names(params_siml)) {
    params_siml$daily_diagnostics <- TRUE
  }

  return(params_siml)
}

prepare_params_siml <- function(params_siml){
  params_siml <- params_siml %>% select(
    "spinup", # Dummy argument
    "spinupyears",
    "recycle",
    "firstyeartrend",
    "nyeartrend",
    "steps_per_day",
    "do_U_shaped_mortality",
    "do_closedN_run",
    "code_method_photosynth",
    "code_method_mortality",
    "daily_diagnostics"
  )

  return(params_siml)
}

build_init_lu <- function(init_lu){
  # If init_lu is null, we create a dummy LU initial state containing only one state (with a fraction of 1)
  if (is.null(init_lu))
    init_lu <- data.frame(name=c('primary'), fraction=c(1.0))

  return(init_lu)
}

prepare_init_lu <- function(init_lu){
  if(!'preset' %in% names(init_lu)) {
    init_lu <- init_lu %>% mutate(preset = 'unmanaged')
  }
  if(!'extra_N_input' %in% names(init_lu)) {
    init_lu <- init_lu %>% mutate('extra_N_input' = case_match(
      'preset',
      "cropland" ~ 0.01,
      .default = 0.0
    ))
  }
  if(!'extra_turnover_rate' %in% names(init_lu)) {
    init_lu <- init_lu %>% mutate('extra_turnover_rate' = case_match(
      'preset',
      "cropland" ~ 0.2,
      .default = 0.0
    ))
  }
  if(!'oxidized_litter_fraction' %in% names(init_lu)) {
    init_lu <- init_lu %>% mutate('oxidized_litter_fraction' = case_match(
      'preset',
      "cropland" ~ 0.9,
      "pasture" ~ 0.4,
      .default = 0.0
    ))
  }
  init_lu <- init_lu %>% mutate(
    'vegetated' = case_match(
      'preset',
      "urban" ~ FALSE,
      .default = TRUE
    )
  )
  init_lu <- init_lu %>% select(
    'fraction',
    'vegetated',
    'extra_N_input',
    'extra_turnover_rate',
    'oxidized_litter_fraction'
  )

  return(init_lu)
}

build_luc_forcing <- function(luc_forcing, n_lu){
  # If luc is null, we create  dummy LU transition matrix containing one all-zero transition
  if (is.null(luc_forcing))
    luc_forcing <- array(rep(0, n_lu ^ 2), c(n_lu, n_lu, 1))
  else if (length(luc_forcing[,1,1]) != n_lu | length(luc_forcing[1,,1]) != n_lu)
    stop(
      paste("run_biomee_f_bysite: the first two dimensions of 'luc_forcing' must be equal to the dimension of 'init_lu'. Got ",
            length(luc_forcing[,1,1]), ' x ', length(luc_forcing[1,,1]), " instead of ", n_lu, ' x ', n_lu, '.')
    )

  return(luc_forcing)
}

prepare_luc_forcing <- function(luc_forcing){
  return(luc_forcing)
}

prepare_forcing <- function(forcing){
  forcing <- forcing %>%
    select(
      "ppfd",
      "temp",
      "vpd",
      "rain",
      "wind",
      "patm",
      "co2"
    )
  return(forcing)
}

prepare_site_info <- function(site_info){
  site_info <- site_info %>% select(
    "lon",
    "lat",
    "elv"
  )
  return(site_info)
}

prepare_init_cohorts <- function(init_cohort){
  if ('init_n_cohorts' %in% names(init_cohort)) {
    warning("Warning: Ignoring column 'init_n_cohorts' under 'init_cohort' in drivers. It has been phased out and should be removed from drivers.")
  }

  if(!'lu_index' %in% names(init_cohort)) {
    init_cohort <- init_cohort %>% mutate('lu_index' = 0)
  }

  init_cohort <- init_cohort %>% select(
    "init_cohort_species",
    "init_cohort_nindivs",
    "init_cohort_bl",
    "init_cohort_br",
    "init_cohort_bsw",
    "init_cohort_bHW",
    "init_cohort_seedC",
    "init_cohort_nsc",
    "lu_index"
  )

  return(init_cohort)
}

prepare_params_tile <- function(params_tile){
  params_tile <- params_tile %>% select(
    "soiltype",
    "FLDCAP",
    "WILTPT",
    "K1",
    "K2",
    "K_nitrogen",
    "MLmixRatio",
    "etaN",
    "LMAmin",
    "fsc_fine",
    "fsc_wood",
    "GR_factor",
    "l_fract",
    "retransN",
    "f_initialBSW",
    "f_N_add",
    "tf_base",
    "par_mort",
    "par_mort_under"
  )
  return(params_tile)
}

prepare_params_species <- function(params_species){
  params_species <- params_species %>% select(
    "lifeform",
    "phenotype",
    "pt",
    "alpha_FR",
    "rho_FR",
    "root_r",
    "root_zeta",
    "Kw_root",
    "leaf_size",
    "Vmax",
    "Vannual",
    "wet_leaf_dreg",
    "m_cond",
    "alpha_phot",
    "gamma_L",
    "gamma_LN",
    "gamma_SW",
    "gamma_FR",
    "tc_crit",
    "tc_crit_on",
    "gdd_crit",
    "betaON",
    "betaOFF",
    "alphaHT",
    "thetaHT",
    "alphaCA",
    "thetaCA",
    "alphaBM",
    "thetaBM",
    "seedlingsize",
    "maturalage",
    "v_seed",
    "mortrate_d_c",
    "mortrate_d_u",
    "LMA",
    "leafLS",
    "LNbase",
    "CNleafsupport",
    "rho_wood",
    "taperfactor",
    "lAImax",
    "tauNSC",
    "fNSNmax",
    "phiCSA",
    "CNleaf0",
    "CNsw0",
    "CNwood0",
    "CNroot0",
    "CNseed0",
    "Nfixrate0",
    "NfixCost0",
    "internal_gap_frac",
    "kphio",
    "phiRL",
    "LAI_light"
  )
  return(params_species)
}

prepare_init_soil <- function(init_soil){
  init_soil <- init_soil %>% select(
    "init_fast_soil_C",
    "init_slow_soil_C",
    "init_Nmineral",
    "N_input"
  )
}

daily_tile_output <- function(raw_data){
  df <- as.data.frame(raw_data)
  colnames(df) <- c(
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
    "N_uptk"
  )
  return(df)
}

annual_tile_output <- function(raw_data){
  df <- as.data.frame(raw_data)
  colnames(df) <- c(
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
    "c_turnover_time",
    "lu_fraction"
  )
  return(df)
}

aggregated_annual_tile_output <- function(raw_data){
  df <- as.data.frame(raw_data)
  colnames(df) <- c(
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
    "c_turnover_time",
    "lu_fraction",
    "prod_pool_1_C",
    "prod_pool_1_N",
    "prod_pool_2_C",
    "prod_pool_2_N"
  )

  return(df)
}

annual_cohort_output <- function(raw_data){
  annual_values <- c(
    "cohort",
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
    "deathrate",
    "n_deadtrees",
    "c_deadtrees"
  )

  dimensions <- dim(raw_data)
  dim(raw_data) <- c(prod(dimensions[1:2]), dimensions[3])
  df <- as.data.frame(raw_data)
  colnames(df) <- annual_values

  ## drop rows (cohorts) with no values
  df$year[df$year <= 0] <- NA
  df <- df[!is.na(df$year),]

  return(df)
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
