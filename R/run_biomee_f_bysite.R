#' R wrapper for SOFUN biomee
#' 
#' Call to the biomee Fortran model
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param site_info Site meta info
#' @param forcing forcing (input) dataframe (returned object by `prepare_input_sofun()`)
#' @param params_tile Tile-level model parameters
#' @param params_species Species-specific model parameters
#' @param params_soil Soil parameters (per soil layer)
#' @param init_cohort Initial size of individuals in each of the N initally present cohorts
#' @param init_soil Initial soil pool sizes
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#'
#' @details Model output is provided as a tidy dataframe
#'
#' @export
#' @useDynLib rsofun

run_biomee_f_bysite <- function(
  sitename,
  params_siml,
  site_info,
  forcing,
  params_tile,
  params_species,
  params_soil,
  init_cohort,
  init_soil,
  makecheck = TRUE
  ){
  
  # predefine variables for CRAN check compliance
  type <- NULL
  
  # select relevant columns of the forcing data
  forcing <- forcing %>%
    select(
      'year',
      'doy',
      'hour',
      'par',
      'ppfd',
      'temp',
      'temp_soil',
      'rh',
      'prec',
      'wind',
      'patm',
      'co2',
      'swc'
    )
  
  params_soil <- params_soil %>%
    dplyr::select(-type)

  runyears <- ifelse(
    params_siml$spinup,
    (params_siml$spinupyears + params_siml$nyeartrend),
    params_siml$nyeartrend)
  
  n_daily  <- params_siml$nyeartrend * 365

  # Types of photosynthesis model
    if (params_siml$method_photosynth == "gs_leuning"){
    code_method_photosynth = 1
  } else if (params_siml$method_photosynth == "pmodel"){
    code_method_photosynth = 2
    dt_days <- forcing$doy[2] - forcing$doy[1]
    dt_hours <- forcing$hour[2] - forcing$hour[1]
    if (dt_days!=1 && dt_hours != 0){
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
    code_method_mortality = 1
  } else if (params_siml$method_mortality == "growthrate"){
    code_method_mortality = 2
  } else if (params_siml$method_mortality == "dbh"){
    code_method_mortality = 3
  } else if (params_siml$method_mortality == "const_selfthin"){
    code_method_mortality = 4
  } else if (params_siml$method_mortality == "bal"){
    code_method_mortality = 5
  } else {
    stop(
      paste("run_biomee_f_bysite: params_siml$method_mortality not recognised:",
            params_siml$method_mortality))
  }

  # base state, always execute the call
  continue <- TRUE
  
  # validate input
  if (makecheck){
    
    # create a loop to loop over a list of variables
    # to check validity
    
    check_vars <- c(
      "par",
      "ppfd",
      "temp",
      "temp_soil",
      "rh",
      "prec",
      "wind",
      "patm",
      "co2",
      "swc"
    )
    
    data_integrity <- lapply(
      check_vars,
      function(check_var){
        if (any(is.nanull(forcing[check_var]))){
          warning(
            sprintf("Error: Missing value in %s for %s",
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
    lm3out <- .Call(

      'biomee_f_C',

      ## Simulation parameters
      spinup                = as.logical(params_siml$spinup),
      spinupyears           = as.integer(params_siml$spinupyears),
      recycle               = as.integer(params_siml$recycle),
      firstyeartrend        = as.integer(params_siml$firstyeartrend),
      nyeartrend            = as.integer(params_siml$nyeartrend),
      outputhourly          = as.logical(params_siml$outputhourly),
      outputdaily           = as.logical(params_siml$outputdaily),
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
      params_species = as.matrix(params_species),
      
      ## soil parameters
      params_soil = as.matrix(params_soil),
      
      ## initial cohort sizes
      init_cohort = as.matrix(init_cohort),

      ## initial soil pools
      init_fast_soil_C = as.numeric(init_soil$init_fast_soil_C),
      init_slow_soil_C = as.numeric(init_soil$init_slow_soil_C),
      init_Nmineral    = as.numeric(init_soil$init_Nmineral),
      N_input          = as.numeric(init_soil$N_input),
      n                = as.integer(nrow(forcing)), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_daily          = as.integer(n_daily), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_annual         = as.integer(runyears), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      n_annual_cohorts = as.integer(params_siml$nyeartrend), # n here is for hourly (forcing is hourly), add n for daily and annual outputs
      forcing          = as.matrix(forcing)
      )
    
    # If simulation is very long, output gets massive.
    # E.g., In a 3000 years-simulation 'lm3out' is 11.5 GB.
    # In such cases (here, more than 5 GB), ignore hourly and daily outputs at tile and cohort levels
    size_of_object_gb <- as.numeric(
      gsub(
        pattern = " Gb",
        replacement = "",
        format(
          utils::object.size(lm3out), 
          units = "GB"
          )
        )
      )
    
    if (size_of_object_gb >= 5){
      warning(
        sprintf("Warning: Excessive size of output object (%s) for %s. Hourly and daily outputs at tile and cohort levels are not returned.",
                format(
                  utils::object.size(lm3out), 
                  units = "GB"
                ), 
                sitename))
    }
    
    #---- Single level output, one matrix ----
    # hourly
    if (size_of_object_gb < 5){
      output_hourly_tile <- as.data.frame(lm3out[[1]], stringAsFactor = FALSE)
      colnames(output_hourly_tile) <- c("year", "doy", "hour",
                                        "rad", "Tair", "Prcp",
                                        "GPP", "Resp", "Transp",
                                        "Evap", "Runoff", "Soilwater",
                                        "wcl", "FLDCAP", "WILTPT")
    } else {
      output_hourly_tile <- NA
    }
    
    # daily_tile
    if (size_of_object_gb < 5){
      output_daily_tile <- as.data.frame(lm3out[[2]], stringAsFactor = FALSE)
      colnames(output_daily_tile) <- c(
        "year", "doy", "Tc",
        "Prcp", "totWs", "Trsp",
        "Evap", "Runoff", "ws1",
        "ws2", "ws3", "LAI",
        "GPP", "Rauto", "Rh",
        "NSC", "seedC", "leafC",
        "rootC", "SW_C", "HW_C",
        "NSN", "seedN", "leafN",
        "rootN", "SW_N", "HW_N",
        "McrbC", "fastSOM", "slowSOM",
        "McrbN", "fastSoilN", "slowSoilN",
        "mineralN", "N_uptk")
    } else {
      output_daily_tile <- NA
    }
    
    # annual tile
    output_annual_tile <- as.data.frame(lm3out[[30]], stringAsFactor = FALSE)
    colnames(output_annual_tile) <- c("year", "CAI", "LAI",
          "Density", "DBH", "Density12",
          "DBH12", "QMD", "NPP",
          "GPP", "Rauto", "Rh",
          "rain", "SoilWater","Transp",
          "Evap", "Runoff", "plantC",
          "soilC", "plantN", "soilN",
          "totN", "NSC", "SeedC", "leafC",
          "rootC", "SapwoodC", "WoodC",
          "NSN", "SeedN", "leafN",
          "rootN", "SapwoodN", "WoodN",
          "McrbC", "fastSOM", "SlowSOM",
          "McrbN", "fastSoilN", "slowSoilN",
          "mineralN", "N_fxed", "N_uptk",
          "N_yrMin", "N_P2S", "N_loss",
          "totseedC", "totseedN", "Seedling_C",
          "Seedling_N", "MaxAge", "MaxVolume",
          "MaxDBH", "NPPL", "NPPW",
          "n_deadtrees", "c_deadtrees", "m_turnover", 
          "c_turnover_time")
    
    #---- Multi-level output, multiple matrices to be combined ----
    
    # Convert to non dplyr routine, just a lapply looping over
    # matrices converting to vector, this is a fixed format
    # with preset conditions so no additional tidyverse logic
    # is required for the conversion
    #
    # Cohort indices can be formatted using a matrix of the same
    # dimension as the data, enumerated by column and unraveled
    # as vector()

    #---- daily cohorts ----
    if (size_of_object_gb < 5){
      daily_values <- c(
        "year","doy","hour",
        "cID", "PFT", "layer",
        "density","f_layer", "LAI",
        "gpp","resp","transp",
        "NPPleaf","NPProot", "NPPwood", "NSC",
        "seedC", "leafC", "rootC",
        "SW_C", "HW_C", "NSN",
        "seedN", "leafN", "rootN",
        "SW_N", "HW_N"
      )
      output_daily_cohorts <- lapply(1:length(daily_values), function(x){
        loc <- 2 + x
        v <- data.frame(
          as.vector(lm3out[[loc]]),
          stringsAsFactors = FALSE)
        names(v) <- daily_values[x]
        return(v)
      })
      
      output_daily_cohorts <- do.call("cbind", output_daily_cohorts)
      
      cohort <- sort(rep(1:ncol(lm3out[[3]]),nrow(lm3out[[3]])))
      output_daily_cohorts <- cbind(cohort, output_daily_cohorts)
      
      # drop rows (cohorts) with no values
      output_daily_cohorts$year[output_daily_cohorts$year == -9999 |
                                  output_daily_cohorts$year == 0] <- NA
      output_daily_cohorts <- 
        output_daily_cohorts[!is.na(output_daily_cohorts$year),]
    } else {
      output_daily_cohorts <- NA
    }
    
    #--- annual cohorts ----
    annual_values <- c(
      "year","cID",
      "PFT","layer","density",
      "f_layer","dDBH","dbh",
      "height","age","Acrown",
      "wood","nsc","NSN","NPPtr",
      "seed","NPPL","NPPR","NPPW",
      "GPP_yr","NPP_yr","Rauto",
      "N_uptk","N_fix","maxLAI",
      "Volume","n_deadtrees",
      "c_deadtrees","deathrate"
    )
    
    output_annual_cohorts <- lapply(1:length(annual_values), function(x){
      loc <- 30 + x
      v <- data.frame(
        as.vector(lm3out[[loc]]),
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
      output_hourly_tile = output_hourly_tile,
      output_daily_tile = output_daily_tile,
      output_daily_cohorts = output_daily_cohorts,
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
