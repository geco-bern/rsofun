#' R wrapper for SOFUN P-model
#' 
#' Call to the Fortran P-model
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param siteinfo Site meta info
#' @param forcing forcing (input) dataframe 
#'  (returned object by `prepare_input_sofun()`)
#' @param df_soiltexture A list of soil texture parameters
#' @param params_modl Model parameters 
#' @param makecheck A logical specifying whether checks are performed 
#'  to verify forcings.
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
  siteinfo,
  forcing,
  df_soiltexture,
  params_modl,
  makecheck = TRUE
  ){

  # rlang::inform(paste("run_pmodel_f_bysite() for ", sitename))

  ## record first year and number of years in forcing data frame (may need to overwrite later)

  ## determine number of seconds per time step
  times <- forcing %>% pull(date) %>% head(2)
  secs_per_tstep <- difftime(times[1], times[2], units = "secs") %>% as.integer() %>% abs()

  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::mutate(
      netrad = -9999.9,
      fsun = (100-ccov)/100,
      snowf = 0.0,
      ndep = 0.0) %>% 
    dplyr::select(
      temp,
      rain,
      vpd,
      ppfd,
      netrad,
      fsun,
      snowf,
      co2,
      ndep,
      fapar,
      patm
      )
  
  # base state, always execute the call
  continue <- TRUE
  
  # validate input
  if (makecheck){
    
    # create a loop to loop over a list of variables
    # to check validity
    
    check_vars <- c(
      "temp",
      "rain",
      "vpd",
      "ppfd",
      "netrad",
      "fsun",
      "snowf",
      "co2",
      "ndep",
      "fapar",
      "patm"
    )
    
    data_integrity <- lapply(check_vars, function(check_var){
      if (any(is.nanull(forcing[check_var]))){
        rlang::warn(sprintf("Error: Missing value in %s for %s", check_var, sitename))
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    if (nrow(forcing) != params_siml$nyeartrend * 365){
      rlang::warn("Error: Number of years data in forcing does not correspond 
                  to number of simulation years (nyeartrend).")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/365))
      rlang::warn(paste(" Number of simulation years: ",
                        params_siml$nyeartrend))
      rlang::warn(" Returning a dummy data frame.")
    }
    
    if (nrow(forcing) != params_siml$nyeartrend * 365 && !all(data_integrity)){
        continue <- FALSE
      }
  }
  
  if (continue){
    
    # convert to matrix
    forcing <- as.matrix(forcing)
    
    # number of rows in matrix (pre-allocation of memory)
    n <- as.integer(nrow(forcing))

    ## Model parameters as vector
    par = c(
      as.numeric(params_modl$kphio),
      as.numeric(params_modl$soilm_par_a),
      as.numeric(params_modl$soilm_par_b),
      as.numeric(params_modl$tau_acclim_tempstress),
      as.numeric(params_modl$par_shape_tempstress)
      )

    ## Soil texture as matrix (layer x texture parameter)
    soiltexture <- df_soiltexture %>% 
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
      longitude                 = as.numeric(siteinfo$lon),
      latitude                  = as.numeric(siteinfo$lat),
      altitude                  = as.numeric(siteinfo$elv),
      whc                       = as.numeric(siteinfo$whc),
      soiltexture               = soiltexture,
      n                         = n,
      par                       = par, 
      forcing                   = forcing
      )
    
    # Prepare output to be a nice looking tidy data frame (tibble)
    ddf <- init_dates_dataframe(
      yrstart = params_siml$firstyeartrend,
      yrend = siteinfo$year_end,
      noleap = TRUE)

    out <- out %>%
      as.matrix() %>%
      as.data.frame() %>%
            setNames(c("fapar", "gpp", "transp", "latenth", "pet",
             "vcmax", "jmax", "vcmax25", "jmax25", "gs_accl",
             "wscal", "chi", "iwue")) %>%
      #setNames(c("fapar", "gpp", "transp", "latenth", "pet")) %>%
      as_tibble(.name_repair = "check_unique") %>%
      dplyr::bind_cols(ddf,.) %>% 
      dplyr::select(-year_dec)

  } else {
    out <- tibble(date = lubridate::ymd("2000-01-01"),
                  fapar = NA, gpp = NA, transp = NA, latenth = NA, 
                  pet = NA, vcmax = NA, jmax = NA, vcmax25 = NA, 
                  jmax25 = NA, gs_accl = NA, wscal = NA, chi = NA, iwue = NA)
  }
    
  return(out)

}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
