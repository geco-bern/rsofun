#' R wrapper for SOFUN P-model
#' 
#' Call to the Fortran P-model
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param site_info Site meta info
#' @param forcing forcing (input) dataframe 
#'  (returned object by `prepare_input_sofun()`)
#' @param params_soil A list of soil texture parameters
#' @param params_modl Model parameters 
#' @param makecheck A logical specifying whether checks are performed 
#'  to verify forcings.
#' @param verbose A logical specifying whether to print warnings.
#' Defaults to TRUE.
#'
#' @import dplyr
#' 
#' @details Model output is provided as a tidy dataframe
#'
#' @export
#' @useDynLib rsofun
#'
run_pmodel_f_bysite <- function(
  sitename,
  params_siml,
  site_info,
  forcing,
  params_soil,
  params_modl,
  makecheck = TRUE,
  verbose = TRUE
  ){
  
  # predefine variables for CRAN check compliance
  ccov <- temp <- rain <- vpd <- ppfd <- netrad <-
  fsun <- snow <- co2 <- ndep <- fapar <- patm <- 
  tmin <- tmax <- fsand <- fclay <- forg <- fgravel <- . <- NULL
  
  # base state, always execute the call
  continue <- TRUE
  
  # record first year and number of years in forcing data 
  # frame (may need to overwrite later)
  ndayyear <- 365
  
  firstyeartrend_forcing <- forcing %>%
    dplyr::ungroup() %>%
    dplyr::slice(1) %>%
    dplyr::pull(date) %>%
    lubridate::year()
  
  nyeartrend_forcing <- nrow(forcing)/ndayyear
  
  # determine number of seconds per time step
  times <- forcing %>%
    dplyr::pull(date) %>%
    utils::head(2)
  secs_per_tstep <- difftime(times[1], times[2], units = "secs") %>%
    as.integer() %>%
    abs()
  
  # re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::mutate(
      netrad = -9999.9,
      fsun = (100-ccov)/100,
      ndep = 0.0
      ) %>% 
    dplyr::select(
      temp,
      rain,
      vpd,
      ppfd,
      netrad,
      fsun,
      snow,
      co2,
      ndep,
      fapar,
      patm,
      tmin,
      tmax
      )
  
  # validate input
  if (makecheck){
    
    # list variable to check for
    check_vars <- c(
      "temp",
      "rain",
      "vpd",
      "ppfd",
      "netrad",
      "fsun",
      "snow",
      "co2",
      "ndep",
      "fapar",
      "patm",
      "tmin",
      "tmax"
    )
    
    # create a loop to loop over a list of variables
    # to check validity
    
    data_integrity <- lapply(check_vars, function(check_var){
      if (any(is.nanull(forcing[check_var]))){
        warning(sprintf("Error: Missing value in %s for %s",
                            check_var, sitename))
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    if (suppressWarnings(!all(data_integrity))){
      continue <- FALSE
    }
    
    # parameters to check
    check_param <- c(
      "spinup",
      "spinupyears",
      "recycle",
      "firstyeartrend",
      "nyeartrend",
      "soilmstress",
      "tempstress",
      "in_ppfd",
      "in_netrad",
      "outdt",
      "ltre",
      "ltne",
      "ltnd",
      "lgr3",
      "lgn3",
      "lgr4"
    )
    
    parameter_integrity <- lapply(check_param, function(check_var){
      if (any(is.nanull(params_siml[check_var]))){
        warning(sprintf("Error: Missing value in %s for %s",
                            check_var, sitename))
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    if (suppressWarnings(!all(parameter_integrity))){
      continue <- FALSE
    }
    
    # Dates in 'forcing' do not correspond to simulation parameters
    if (nrow(forcing) != params_siml$nyeartrend * ndayyear){
      if (verbose){
        warning(
          "Error: Number of years data in forcing does not correspond
       to number of simulation years (nyeartrend).\n")
        warning(paste(" Number of years data: ",
                          nrow(forcing)/ndayyear), "\n")
        warning(paste(" Number of simulation years: ",
                          params_siml$nyeartrend, "\n"))
        warning(paste(" Site name: ", sitename, "\n"))
      }
      
      # Overwrite params_siml$nyeartrend and 
      # params_siml$firstyeartrend based on 'forcing'
      if (nrow(forcing) %% ndayyear == 0){
        params_siml$nyeartrend <- nyeartrend_forcing
        params_siml$firstyeartrend <- firstyeartrend_forcing
        if (verbose){
          warning(paste(" Overwriting params_siml$nyeartrend: ",
                            params_siml$nyeartrend, "\n"))
          warning(paste(" Overwriting params_siml$firstyeartrend: ",
                            params_siml$firstyeartrend, "\n"))
        }
      } else {
        # something weird more fundamentally -> don't run the model
        warning(" Returning a dummy data frame.")
        continue <- FALSE
      }
    }
  }
  
  if (continue){
    
    # convert to matrix
    forcing <- as.matrix(forcing)
    
    # number of rows in matrix (pre-allocation of memory)
    n <- as.integer(nrow(forcing))

    # Model parameters as vector
    par = c(
      as.numeric(params_modl$kphio),
      as.numeric(params_modl$soilm_par_a),
      as.numeric(params_modl$soilm_par_b),
      as.numeric(params_modl$tau_acclim_tempstress),
      as.numeric(params_modl$par_shape_tempstress),
      as.numeric(params_modl$whc)
      )

    # Soil texture as matrix (layer x texture parameter)
    soiltexture <- params_soil %>% 
      dplyr::select(fsand, fclay, forg, fgravel) %>% 
      as.matrix() %>% 
      t()

    ## C wrapper call
    out <- .Call(

      'pmodel_f_C',
      
      ## Simulation parameters
      spinup                    = as.logical(params_siml$spinup),
      spinupyears               = as.integer(params_siml$spinupyears),
      recycle                   = as.integer(params_siml$recycle),
      firstyeartrend            = as.integer(params_siml$firstyeartrend),
      nyeartrend                = as.integer(params_siml$nyeartrend),
      secs_per_tstep            = as.integer(secs_per_tstep),
      soilmstress               = as.logical(params_siml$soilmstress),
      tempstress                = as.logical(params_siml$tempstress),
      in_ppfd                   = as.logical(params_siml$in_ppfd),
      in_netrad                 = as.logical(params_siml$in_netrad),
      outdt                     = as.integer(params_siml$outdt),
      ltre                      = as.logical(params_siml$ltre),
      ltne                      = as.logical(params_siml$ltne),
      ltrd                      = as.logical(params_siml$ltrd),
      ltnd                      = as.logical(params_siml$ltnd),
      lgr3                      = as.logical(params_siml$lgr3),
      lgn3                      = as.logical(params_siml$lgn3),
      lgr4                      = as.logical(params_siml$lgr4),
      longitude                 = as.numeric(site_info$lon),
      latitude                  = as.numeric(site_info$lat),
      altitude                  = as.numeric(site_info$elv),
      whc                       = as.numeric(site_info$whc),
      soiltexture               = soiltexture,
      n                         = n,
      par                       = par, 
      forcing                   = forcing
      )
    
    # Prepare output to be a nice looking tidy data frame (tibble)
    ddf <- init_dates_dataframe(
      yrstart = params_siml$firstyeartrend,
      yrend = params_siml$firstyeartrend + params_siml$nyeartrend - 1,
      noleap = TRUE)

    out <- out %>%
      as.matrix() %>% 
      as.data.frame() %>% 
      stats::setNames(
        c("fapar", "gpp", "transp", "latenth", "pet", "vcmax",
          "jmax", "vcmax25", "jmax25", "gs_accl", "wscal", "chi", "iwue", "rd")
        ) %>%
      as_tibble(.name_repair = "check_unique") %>%
      dplyr::bind_cols(ddf,.)

  } else {
    out <- tibble(date = lubridate::ymd("2000-01-01"),
                  fapar = NA, gpp = NA, transp = NA, latenth = NA, 
                  pet = NA, vcmax = NA, jmax = NA, vcmax25 = NA, 
                  jmax25 = NA, gs_accl = NA, wscal = NA, chi = NA, iwue = NA, rd = NA)
  }
    
  return(out)

}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
