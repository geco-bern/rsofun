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
#' @param luc_forcing An array of land use change (LUC) used during transient phase.
#'
#' For further specifications of above inputs and examples see \code{\link{biomee_gs_leuning_drivers}}, \code{\link{biomee_p_model_drivers}}, or \code{\link{biomee_p_model_luluc_drivers}}.

#' @import lubridate 
#' 
#' @returns A data.frame with columns containing model output for each land unit (LU). 
#' See examples \code{\link{biomee_gs_leuning_output}}, \code{\link{biomee_p_model_output}}, or \code{\link{biomee_p_model_luluc_output}}. 
#' If only one land unit (LU) is simulated, the column is named 'data'.
#' If multiple land units (LU) are simulated, the columns are named according to the LU names.
#' If multiple land units (LU) are simulated, an additional column 'aggregated' contains output aggregating all tiles as 
#' well as product pools.
#' Model output for each land unit (LU) is provided as a list. 
#' Each list has elements: \code{output_daily_tile}, \code{output_annual_tile}, and \code{output_annual_cohorts}.
#' Model output for the aggregated land units (LU) is provided as a list containing \code{output_daily_cell}.
#' \describe{
#'   \item{\code{output_daily_tile}}{A data.frame with daily outputs at tile level.
#'     \describe{
#'       \item{year}{Year of the simulation.}
#'       \item{doy}{Day of the year.}
#'       \item{Tk}{Air temperature (Kelvin).}
#'       \item{Prcp}{Precipitation (mm m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{SoilWater}{Soil water content in root zone (kg m\eqn{^{-2}}).}
#'       \item{Transp}{Transpiration (mm m\eqn{^{2-}} day\eqn{^{-1}}).}
#'       \item{Evap}{Evaporation (mm m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{Runoff}{Water runoff (mm m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{ws1}{Volumetric soil water content for layer 1.}
#'       \item{ws2}{Volumetric soil water content for layer 2.}
#'       \item{ws3}{Volumetric soil water content for layer 3.}
#'       \item{LAI}{Leaf area index (m\eqn{^2}/m\eqn{^2}).}
#'       \item{NPP}{Net primary productivity (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{GPP}{Gross primary production (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{Rauto}{Plant autotrophic respiration (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{Rh}{Heterotrophic respiration (kg C m\eqn{^{-2}} day\eqn{^{-1}}).}
#'       \item{NSC}{Non-structural carbon (kg C m\eqn{^{-2}}).}
#'       \item{seedC}{Biomass of seeds (kg C m\eqn{^{-2}}).}
#'       \item{leafC}{Biomass of leaves (kg C m\eqn{^{-2}}).}
#'       \item{rootC}{Biomass of fine roots (kg C m\eqn{^{-2}}).}
#'       \item{sapwoodC}{Biomass of sapwood (kg C m\eqn{^{-2}}).}
#'       \item{heartwoodC}{Biomass of heartwood (kg C m\eqn{^{-2}}).}
#'       \item{NSN}{Non-structural N pool (kg N m\eqn{^{-2}}).}
#'       \item{seedN}{Nitrogen of seeds (kg N m\eqn{^{-2}}).}
#'       \item{leafN}{Nitrogen of leaves (kg N m\eqn{^{-2}}).}
#'       \item{rootN}{Nitrogen of roots (kg N m\eqn{^{-2}}).}
#'       \item{sapwoodN}{Nitrogen of sapwood (kg N m\eqn{^{-2}}).}
#'       \item{heartwoodN}{Nitrogen of heartwood (kg N m\eqn{^{-2}}).}
#'       \item{mcrbC}{Microbial carbon (kg C m\eqn{^{-2}}).}
#'       \item{fastSOM}{Fast soil carbon pool (kg C m\eqn{^{-2}}).}
#'       \item{slowSOM}{Slow soil carbon pool (kg C m\eqn{^{-2}}).}
#'       \item{mcrbN}{Microbial nitrogen (kg N m\eqn{^{-2}}).}
#'       \item{fastSoilN}{Fast soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{slowSoilN}{Slow soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{mineralN}{Mineral nitrogen pool (kg N m\eqn{^{-2}}).}
#'       \item{N_uptk}{Nitrogen uptake (kg N m\eqn{^{-2}} day\eqn{^{-1}}).}
#'     }}
#'   \item{\code{output_annual_tile}}{A data.frame with annual outputs at tile level.
#'   \describe{
#'     \item{year}{Year of the simulation.}
#'     \item{CAI}{Crown area index (m\eqn{^2}/m\eqn{^2}).}
#'     \item{LAI}{Leaf area index (m\eqn{^2}/m\eqn{^2}).}
#'     \item{Density}{Number of trees per area (trees ha\eqn{^{-1}}).}
#'     \item{DBH}{Diameter at tile level (cm).}
#'     \item{Density12}{Tree density for trees with DBH > 12 cm (individuals ha\eqn{^{-1}}).}
#'     \item{DBH12}{Diameter at tile level considering trees with DBH > 12 cm(cm).}
#'     \item{QMD12}{Quadratic mean diameter at tile level considering trees withDBH > 12 cm (cm).}
#'     \item{NPP}{Net primary productivity (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{GPP}{Gross primary productivity (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Rauto}{Plant autotrophic respiration (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Rh}{Heterotrophic respiration (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Prcp}{Annual precipitation (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{SoilWater}{Soil water content in root zone (kg m\eqn{^{-2}}).}
#'     \item{Transp}{Transpiration (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Evap}{Evaporation (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{Runoff}{Water runoff (mm m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{plantC}{Plant biomass (kg C m\eqn{^{-2}}).}
#'     \item{soilC}{Soil carbon (kg C m\eqn{^{-2}}).}
#'     \item{totC}{Total carbon in plant and soil (kg C m\eqn{^{-2}}).}
#'     \item{plantN}{Plant nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{soilN}{Soil nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{totN}{Total nitrogen in plant and soil (kg N m\eqn{^{-2}}).}
#'     \item{NSC}{Nonstructural carbohydrates (kg C m\eqn{^{-2}}).}
#'     \item{seedC}{Seed biomass (kg C m\eqn{^{-2}}).}
#'     \item{leafC}{Leaf biomass (kg C m\eqn{^{-2}}).}
#'     \item{rootC}{Fine root biomass (kg C m\eqn{^{-2}}).}
#'     \item{sapwoodC}{Sapwood biomass (kg C m\eqn{^{-2}}).}
#'     \item{heartwoodC}{Heartwood biomass (kg C m\eqn{^{-2}}).}
#'     \item{NSN}{Nonstructural nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{seedN}{Seed nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{leafN}{Leaf nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{rootN}{Fine root nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{sapwoodN}{Sapwood nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{heartwoodN}{Heartwood nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{mcrbC}{Microbial carbon (kg C m\eqn{^{-2}}).}
#'     \item{fastSOM}{Fast soil carbon pool (kg C m\eqn{^{-2}}).}
#'     \item{slowSOM}{Slow soil carbon pool (kg C m\eqn{^{-2}}).}
#'     \item{mcrbN}{Microbial nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{fastSoilN}{Fast soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{slowsoilN}{Slow soil nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{mineralN}{Mineral nitrogen pool (kg N m\eqn{^{-2}}).}
#'     \item{N_fxed}{Nitrogen fixation (kg N m\eqn{^{-2}}).}
#'     \item{N_uptk}{Nitrogen uptake (kg N m\eqn{^{-2}}).}
#'     \item{N_yrMin}{Annual available nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{N_P2S}{Annual nitrogen from plants to soil (kg N m\eqn{^{-2}}).}
#'     \item{N_loss}{Annual nitrogen loss (kg N m\eqn{^{-2}}).}
#'     \item{totseedC}{Total seed carbon (kg C m\eqn{^{-2}}).}
#'     \item{totseedN}{Total seed nitrogen (kg N m\eqn{^{-2}}).}
#'     \item{Seedling_C}{Total carbon from all compartments but seeds (kg C m\eqn{^{-2}}).}
#'     \item{Seedling_N}{Total nitrogen from all compartments but seeds(kg N m\eqn{^{-2}}).}
#'     \item{MaxAge}{Age of the oldest tree in the tile (years).}
#'     \item{MaxVolume}{Maximum volume of a tree in the tile (m\eqn{^3}).}
#'     \item{MaxDBH}{Maximum DBH of a tree in the tile (m).}
#'     \item{NPPL}{Growth of a tree, including carbon allocated to leaves(kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{NPPW}{Growth of a tree, including carbon allocated to sapwood(kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{n_deadtrees}{Number of trees that died (trees m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{c_deadtrees}{Carbon biomass of trees that died (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{m_turnover}{Continuous biomass turnover (kg C m\eqn{^{-2}} yr\eqn{^{-1}}).}
#'     \item{c_turnover_time}{Carbon turnover rate, calculated as the ratio between plant biomass and NPP (yr\eqn{^{-1}}).}
#'     \item{lu_fraction}{Fraction of BiomeE grid cell that is occupied by this land unit (LU tile) tile (unitless, or m\eqn{^{-2}} LU area per m\eqn{^{-2}} grid cell area).}
#'   }}
#'   \item{\code{output_annual_cohorts}}{A data.frame of annual outputs at the cohort level.
#'   \describe{
#'     \item{year}{Year of the simulation.}
#'     \item{cID}{An integer indicating the cohort identity.}
#'     \item{PFT}{An integer indicating the Plant Functional Type.}
#'     \item{layer}{An integer indicating the crown layer, numbered from top to bottom.}
#'     \item{density}{Number of trees per area (trees ha\eqn{^{-1}}).}
#'     \item{flayer}{Fraction of layer area occupied by this cohort.}
#'     \item{DBH}{Tree diameter (cm).}
#'     \item{dDBH}{Diameter growth of a tree in this cohort (cm yr\eqn{^{-1}}).}
#'     \item{height}{Tree height (m).}
#'     \item{age}{Age of the cohort (years).}
#'     \item{BA}{Basal area a tree in this cohort (m\eqn{^2} tree\eqn{^{-1}}).}         
#'     \item{dBA}{Basal area increment of a tree in this cohort (m\eqn{^2} tree\eqn{^{-1}} yr\eqn{^{-1}}).}         
#'     \item{Acrown}{Crown area of a tree in this cohort (m\eqn{^2} tree\eqn{^{-1}}).}
#'     \item{Aleaf}{Total area of leaves (m\eqn{^2} tree\eqn{^{-1}}).}
#'     \item{wood}{Sum of sapwood and heartwood biomass of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{NSC}{Non-structural carbon of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{seedC}{Biomass of seeds of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{leafC}{Biomass of leaves of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{rootC}{Biomass of fine roots of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{sapwoodC}{Biomass of sapwood of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{heartwoodC}{Biomass of heartwood of a tree in this cohort (kg C tree\eqn{^{-1}}).}
#'     \item{NSN}{Non-structural nitrogen of a tree in this cohort (kg N tree\eqn{^{-1}}).}
#'     \item{treeG}{Total growth of a tree, including carbon allocated to seeds, leaves, fine roots, and sapwood (kg C tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{fseed}{Fraction of carbon allocated to seeds to total growth.}
#'     \item{fleaf}{Fraction of carbon allocated to leaves to total growth.}
#'     \item{froot}{Fraction of carbon allocated to fine roots to total growth.}
#'     \item{fwood}{Fraction of carbon allocated to sapwood to total growth.}
#'     \item{NPP}{Net primary productivity of a tree (kg C tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{GPP}{Gross primary productivity of a tree (kg C tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{Rauto}{Plant autotrophic respiration (kg C tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{N_uptk}{Nitrogen uptake (kg N tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{N_fxed}{Nitrogen fixation (kg N tree\eqn{^{-1}} yr\eqn{^{-1}}).}
#'     \item{deathrate}{Mortality rate of this cohort, including natural mortality, starvation and any other processes causing a loss of individuals in general (yr\eqn{^{-1}}).}
#'     \item{n_deadtrees}{Plant to soil N flux due to mortality, including natural mortality, starvation and any other processes causing a loss of individuals in general  (kg N yr\eqn{^{-1}} m\eqn{^{-2}}).}
#'     \item{c_deadtrees}{Plant to soil C flux due to mortality, including natural mortality, starvation and any other processes causing a loss of individuals in general  (kg C yr\eqn{^{-1}} m\eqn{^{-2}}).}
#'   }}
#' }
#' If there are multiple land units (LU) there will also be a column named `aggregated` containing a data.frame in the column
#' `output_annual_cell` with annual outputs aggregating all tiles present in the simulation cell. Note that quantities per m2 refer to 
#' m2 of grid cell area, i.e. the full area of the BiomeE simulation. 'lu_fraction' refers to the sum of all the tiles, 
#' which must remain constant and which represents the fraction of the cell area that is not water/ice. 
#' In most cases, it would be close to 1. It contains columns:
#' \describe{
#'   \item{\code{output_annual_cell}}{A data.frame with annual outputs aggregating all tiles present in the simulation cell. Note that quantities per m\eqn{^{2}} refer to m\eqn{^{2}} of grid cell 
#'                                    area, i.e. the full area of the BiomeE simulation. 'lu_fraction' refers to the sum of all the tiles, which must remain constant and which represents the 
#'                                    fraction of the cell area that is not water/ice. In most cases, it would be close to 1.
#'     \describe{
#'     \item{all columns from 'output_yearly_tile'}{See above for output_yearly_tile, but now expressed per unit area of the BiomeE grid cell.}
#'     \item{lu_fraction}{Fraction of BiomeE grid cell that is occupied by this land unit (LU tile) tile (unitless, or m\eqn{^{2}} LU area per m\eqn{^{2}} grid cell area).}
#'     \item{prod_pool_1_C}{Carbon in product pool 1 (kg C m\eqn{^{-2}} grid cell).}
#'     \item{prod_pool_1_N}{Nitrogen in product pool 1 (kg N m\eqn{^{-2}} grid cell).}
#'     \item{prod_pool_2_C}{Carbon in product pool 2 (kg C m\eqn{^{-2}} grid cell).}
#'     \item{prod_pool_2_N}{Nitrogen in product pool 2 (kg N m\eqn{^{-2}} grid cell).}
#'     \item{Rprod_0_C}{Carbon loss rate directly from land use change (LUC) (kg C m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'     \item{Rprod_0_N}{Nitrogen loss rate directly from land use change (LUC) (kg C m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'     \item{Rprod_1_C}{Carbon loss rate from product pool 1 (kg C m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'     \item{Rprod_1_N}{Nitrogen loss rate from product pool 1 (kg N m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'     \item{Rprod_2_C}{Carbon loss rate from product pool 2 (kg C m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'     \item{Rprod_2_N}{Nitrogen loss rate from product pool 2 (kg N m\eqn{^{-2}} grid cell yr\eqn{^{-1}}).}
#'   }}
#' }
#' 
#' @export
#' @useDynLib rsofun
#'
#' @examples
#' \donttest{ # do not run long-running simulations
#' # Example BiomeE model run
#' 
#' # Use example drivers data
#' drivers <- biomee_p_model_drivers
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
  makecheck = TRUE
){
  ndayyear <- 365

  # Default value for tc_home
  if ("tc_home" %in% names(site_info)) {
    stop("Unexpectedly received site_info$tc_home; it should be calculated internally.")
  }

  conditionally_add_tmax <- function(df){
    need_to_add_tmax <- !("tmax" %in% colnames(df))
    if (need_to_add_tmax) {
      df %>% dplyr::group_by(.data$date) %>%
        dplyr::summarise(daily_tmax = max(.data$temp, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else {
      df %>% dplyr::rename(daily_tmax = "tmax")
    }
  }
  tc_home <- forcing %>%
    # conditionally add daily max temp (if needed, e.g. when running "gs_leuning" with hourly forcing)
    conditionally_add_tmax() %>%
    # add grouping variables:
    mutate(month = lubridate::month(.data$date), year = lubridate::year(.data$date)) %>%
    # monthly means of daily maximum:
    group_by(.data$year, .data$month) %>%
    summarise(monthly_avg_daily_tmax = mean(.data$daily_tmax, na.rm = TRUE), .groups = "drop") %>%
    # warmest month of each year:
    group_by(year) %>%
    summarise(t_warmest_month = max(.data$monthly_avg_daily_tmax)) %>%
    # mean of yearly warmest months:
    ungroup() %>%
    summarise(tc_home = mean(.data$t_warmest_month, na.rm = TRUE)) %>%
    # extract scalar value
    dplyr::pull(.data$tc_home)

    site_info$tc_home <- tc_home     # TODO: rather in site_info or in params_tile?
    # params_tile$tc_home <- tc_home # TODO: rather in site_info or in params_tile?
    
  # Validate calculation
  if (is.na(site_info$tc_home) || length(site_info$tc_home) == 0) {
    warning("Calculated tc_home is NA or missing; defaulting to 25C.")
    site_info$tc_home <- 25
  }

  # record number of years in forcing data
  # frame to use as default values (unless provided othrwise as params_siml$nyeartrend)
  forcing_years <- nrow(forcing)/(ndayyear * params_siml$steps_per_day)

  # Add default parameters (backward compatibility layer)
  params_siml <- build_params_siml(params_siml, forcing_years, makecheck)

  # Build LULUC parameters
  init_lu     <- build_init_lu(init_lu)
  luc_forcing <- build_luc_forcing(luc_forcing, nrow(init_lu))

  if (length(luc_forcing[1,1,]) > params_siml$nyeartrend)
    warning(paste("Warning: 'luc_forcing' contains more data points than nyeartrend (", length(luc_forcing[1,1,]), ' vs ', params_siml$nyeartrend, '). Extra data points will be ignored.'))

  # Set up variables used by C wrapper to build output arrays
  n_daily  <- ifelse(params_siml$do_daily_diagnostics, params_siml$nyeartrend * ndayyear, 0)
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

  out <- build_out_biomee(biomeeout, init_lu$name, sitename, params_siml$do_daily_diagnostics)

  return(out)
}

# Build R output
build_out_biomee <- function(biomeeout, lu_names, sitename, do_daily_diagnostics){
  if (do_daily_diagnostics) {
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
    # first column for aggregated across all LU
    out <- list("aggregated" = list(
      output_annual_cell = annual_tile_output(biomeeout[[4]], aggregated_LU = TRUE)
      # output_daily_tile = NA,
      # output_daily_tile = NA,
      # output_annual_cohorts = NA
    ))
    # following columns for each LU
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

  if ('do_daily_diagnostics' %nin% names(params_siml)) {
    params_siml$do_daily_diagnostics <- TRUE
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
    "do_daily_diagnostics"
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
    dplyr::mutate(d13c_atm = -8.4) %>% # xxx demo: hold constant. should be provided by user
    select(
      "ppfd",
      "temp",
      "vpd",
      "rain",
      "wind",
      "patm",
      "co2",
      "d13c_atm"
    )
  return(forcing)
}

prepare_site_info <- function(site_info){
  site_info <- site_info %>% select(
    "lon",
    "lat",
    "elv",
    "tc_home"
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
    "tk_crit",
    "tk_crit_on",
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
    "Tk",
    "Prcp",
    "SoilWater",
    "Transp",
    "Evap",
    "Runoff",
    "ws1",
    "ws2",
    "ws3",
    "LAI",
    "NPP",
    "GPP",
    "Rauto",
    "Rh",
    "NSC",
    "seedC",
    "leafC",
    "rootC",
    "sapwoodC",
    "heartwoodC",
    "NSN",
    "seedN",
    "leafN",
    "rootN",
    "sapwoodN",
    "heartwoodN",
    "mcrbC",
    "fastSOM",
    "slowSOM",
    "mcrbN",
    "fastSoilN",
    "slowSoilN",
    "mineralN",
    "N_uptk"
  )
  return(df)
}

annual_tile_output <- function(raw_data, aggregated_LU = FALSE){
  df <- as.data.frame(raw_data)
  col_names <- c(
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
    "Prcp",
    "SoilWater",
    "Transp",
    "Evap",
    "Runoff",
    "plantC",
    "soilC",
    "totC",
    "plantN",
    "soilN",
    "totN",
    "NSC",
    "seedC",
    "leafC",
    "rootC",
    "sapwoodC",
    "heartwoodC",
    "NSN",
    "seedN",
    "leafN",
    "rootN",
    "sapwoodN",
    "heartwoodN",
    "mcrbC",
    "fastSOM",
    "slowSOM",
    "mcrbN",
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
  if(aggregated_LU){
    col_names <- c(col_names, 
                   "prod_pool_1_C", 
                   "prod_pool_1_N", 
                   "prod_pool_2_C", 
                   "prod_pool_2_N",
                   "Rprod_0_C", 
                   "Rprod_0_N", 
                   "Rprod_1_C", 
                   "Rprod_1_N", 
                   "Rprod_2_C", 
                   "Rprod_2_N")
  }
  colnames(df) <- col_names
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
    "NSC",
    "seedC",
    "leafC",
    "rootC",
    "sapwoodC",
    "heartwoodC",
    "NSN",
    "treeG",
    "fseed",
    "fleaf",
    "froot",
    "fwood",
    "NPP",
    "GPP",
    "Rauto",
    "N_uptk",
    "N_fxed",
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

  # remove confusing cohort column: 
  # #' \item{cohort}{An index of the cohorts (unused, since this changes from year to year.)}
  df <- df[ ,"cohort" != colnames(df)]
  
  return(df)
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
