#' R wrapper for SOFUN
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param siteinfo Site meta info
#' @param forcing forcing (input) dataframe (returned object by \link{prepare_input_sofun})
#' @param params_tile Tile-level model parameters
#' @param params_species Species-specific model parameters
#' @param params_soil Soil parameters (per soil layer)
#' @param init_cohort Initial size of individuals in each of the N initally present cohorts
#' @param init_soil Initial soil pool sizes
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#'
#' @details This is the model
#'
#' @export
#' @useDynLib rsofun
#'
run_lm3ppa_f_bysite <- function( sitename, params_siml, siteinfo, forcing, params_tile, params_species, params_soil, init_cohort, init_soil, makecheck = TRUE ){


  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::select(1:13)

  params_soil <- params_soil %>%
    dplyr::select(-type)

  ## Tests XXX todo: adapt test to forcing for lm3ppa
  do_continue <- TRUE

  if (makecheck){
    is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
    if (any(is.nanull(forcing$PAR))){
      rlang::warn(paste("Error: Missing value in PAR for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$Swdown))){
      rlang::warn(paste("Error: Missing value in Swdown for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$TEMP))){
      rlang::warn(paste("Error: Missing value in TEMP for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$SoilT))){
      rlang::warn(paste("Error: Missing value in SoilT for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$RH))){
      rlang::warn(paste("Error: Missing value in RH for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$RAIN))){
      rlang::warn(paste("Error: Missing value in RAIN for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$WIND))){
      rlang::warn(paste("Error: Missing value in WIND for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$PRESSURE))){
      rlang::warn(paste("Error: Missing value in PRESSURE for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$aCO2_AW))){
      rlang::warn(paste("Error: Missing value in aCO2_AW for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$amb_co2))){
      rlang::warn(paste("Error: Missing value in amb_co2 for site", sitename, "\n"))
      do_continue <- FALSE
    }
    
    if (nrow(forcing) != params_siml$nyeartrend * 365 * 24){    # xxx todo: must be *48 for lm3ppa
      rlang::warn("Error: Number of years data in forcing does not correspond to number of simulation years (nyeartrend).")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/(365*24)))
      rlang::warn(paste(" Number of simulation years: ", params_siml$nyeartrend))
      rlang::warn(" Returning a dummy data frame.")
    }
  }

  if (do_continue){

    # ## Soil texture as matrix (layer x texture parameter)
    # soiltexture <- df_soiltexture %>% 
    #   dplyr::select(fsand, fclay, forg, fgravel) %>% 
    #   as.matrix() %>% 
    #   t()

    ## C wrapper call
    out <- .Call(

      'lm3ppa_f_C',
      
      ## Simulation parameters
      # filepath_in           = as.character(params_siml$filepath_in),
      # climfile              = as.character(params_siml$climfile),
      model_run_years       = as.integer(params_siml$model_run_years),
      equi_days             = as.integer(params_siml$equi_days),
      outputhourly          = as.logical(params_siml$outputhourly),
      outputdaily           = as.logical(params_siml$outputdaily),
      do_U_shaped_mortality = as.logical(params_siml$do_U_shaped_mortality),
      update_annaulLAImax   = as.logical(params_siml$update_annaulLAImax),
      do_closedN_run        = as.logical(params_siml$do_closedN_run),

      ## site meta info
      longitude             = as.numeric(siteinfo$lon),
      latitude              = as.numeric(siteinfo$lat),
      altitude              = as.numeric(siteinfo$elv),

      ## Tile-level parameters
      soiltype     = as.integer(params_tile$soiltype),
      FLDCAP       = as.numeric(params_tile$FLDCAP),
      WILTPT       = as.numeric(params_tile$WILTPT),
      K1           = as.numeric(params_tile$K1),
      K2           = as.numeric(params_tile$K2),
      K_nitrogen   = as.numeric(params_tile$K_nitrogen),
      etaN         = as.numeric(params_tile$etaN),
      MLmixRatio   = as.numeric(params_tile$MLmixRatio),
      l_fract      = as.numeric(params_tile$l_fract),
      retransN     = as.numeric(params_tile$retransN),
      fNSNmax      = as.numeric(params_tile$fNSNmax),
      f_N_add      = as.numeric(params_tile$f_N_add),
      f_initialBSW = as.numeric(params_tile$f_initialBSW),

      ## Species-specific parameters
      params_species = as.matrix(params_species),

      ## soil parameters
      params_soil = as.matrix(params_soil),

      ## initial cohort sizes
      init_cohort = as.matrix(init_cohort),

      ## initial soil pools
      init_fast_soil_C    = as.numeric(init_soil$init_fast_soil_C),
      init_slow_soil_C    = as.numeric(init_soil$init_slow_soil_C),
      init_Nmineral       = as.numeric(init_soil$init_Nmineral),
      N_input             = as.numeric(init_soil$N_input),

      n       = as.integer(nrow(forcing)),
      forcing = as.matrix(forcing)
      )
    
    ## Prepare output to be a nice looking tidy data frame (tibble)
    ddf <- init_dates_dataframe(yrstart = params_siml$firstyeartrend, yrend = siteinfo$year_end, noleap = TRUE)

    out <- out %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("fapar", "gpp", "transp", "latenth", "XXX")) %>%
      dplyr::mutate(sitename = sitename) %>% 
      dplyr::bind_cols(ddf,.) %>% 
      dplyr::select(-year_dec)

  } else {
    out <- tibble(sitename = sitename, date = lubridate::ymd("2000-01-01"), fapar = NA, gpp = NA, transp = NA, latenth = NA, XXX = NA)
  }
    
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
