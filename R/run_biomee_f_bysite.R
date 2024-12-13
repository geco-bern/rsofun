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
#' @param init_lu Initial land use state array used as input (optional).
#' See examples \code{\link{biomee_gs_leuning_drivers}} or \code{\link{biomee_p_model_drivers}}
#' @param luc Land use change (transition matrix) data.frame used as input (optional).
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
#' @returns Model output. See examples \code{\link{biomee_gs_leuning_output}} or \code{\link{biomee_p_model_output}}.
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
    makecheck = TRUE,
    init_lu = NULL,
    luc = NULL
){
  forcing_features <- c(
    'ppfd',
    'temp',
    'vpd',
    'rain',
    'wind',
    'patm',
    'co2'
  )

  # If init_lu is null, we create dummy LU initial state containing only one state (with a fraction of 1)
  if (is.null(init_lu))
    init_lu <- c(1.0)

  # Number of LU states
  n_lu <- length(init_lu)

  # Number of LU transitions
  # Note: we assume any state can transition to any other state.
  # This simplifies greatly the implementation of a generic solution on the Fortran side
  n_lu_tr <- n_lu ^ 2

  # If luc is null, we create  dummy LU transition matrix containing one all-zero transition
  if (is.null(luc))
    luc <- data.frame(rep(0.0, n_lu_tr))

  # select relevant columns of the forcing data
  forcing <- forcing %>%
    select(
      any_of(forcing_features)
    )
  forcing_years <- nrow(forcing)/(365 * params_siml$steps_per_day)

  `%nin%` <- Negate(`%in%`)
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

  runyears <- ifelse(
    params_siml$spinup,
    (params_siml$spinupyears + params_siml$nyeartrend),
    params_siml$nyeartrend
  )
  
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

    if ('init_n_cohorts' %in% names(init_cohort)) {
      warning("Error: column 'init_n_cohorts' under 'init_cohort' has been phased out and must be removed from the drivers.")
      data_integrity <- append(data_integrity, FALSE)
    }
    
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
      steps_per_day         = as.integer(params_siml$steps_per_day),
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
      
      ## initial cohort
      init_cohort = as.matrix(init_cohort),
      
      ## initial soil pools
      init_fast_soil_C = as.numeric(init_soil$init_fast_soil_C),
      init_slow_soil_C = as.numeric(init_soil$init_slow_soil_C),
      init_Nmineral    = as.numeric(init_soil$init_Nmineral),
      N_input          = as.numeric(init_soil$N_input),
      n_daily          = as.integer(n_daily), 
      n_annual         = as.integer(runyears), 
      n_annual_cohorts = as.integer(params_siml$nyeartrend), # to get cohort outputs after spinup year
      #n_annual_cohorts = as.integer(runyears), # to get cohort outputs from year 1
      forcing          = as.matrix(forcing),
      lu               = as.vector(init_lu),
      luc              = as.matrix(luc)
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
      "c_turnover_time",
      "lu_fraction"
    )
    
    #--- annual cohorts ----
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
      "n_deadtrees",
      "c_deadtrees",
      "deathrate"
    )

    data <- biomeeout[[3]]
    data_flatten <- matrix(data, prod(dim(data)[1:2]), dim(data)[3])
    output_annual_cohorts <- as.data.frame(data_flatten, stringAsFactor = FALSE)
    colnames(output_annual_cohorts) <- annual_values

    ## drop rows (cohorts) with no values
    output_annual_cohorts$year[output_annual_cohorts$year == -9999 |
                                 output_annual_cohorts$year == 0] <- NA
    output_annual_cohorts <- output_annual_cohorts[!is.na(output_annual_cohorts$year),]
    
    # format the output in a structured list
    out <- list(
      output_daily_tile = output_daily_tile,
      output_annual_tile = output_annual_tile,
      output_annual_cohorts = output_annual_cohorts
    )
    
  } else {
    out <- NA
  }
  
  return(out)
  
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
