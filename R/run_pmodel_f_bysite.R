#' R wrapper for SOFUN
#'
#' @param sitename Site name
#' @param params_siml Simulation parameters
#' @param siteinfo Site meta info
#' @param forcing forcing (input) dataframe (returned object by \link{prepare_input_sofun})
#' @param df_soiltexture A list of soil texture parameters
#' @param params_modl Model parameters 
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#'
#' @details This is the model
#'
#' @export
#' @useDynLib rsofun
#'
run_pmodel_f_bysite <- function( sitename, params_siml, siteinfo, forcing, df_soiltexture, params_modl, makecheck = TRUE ){

  # rlang::inform(paste("run_pmodel_f_bysite() for ", sitename))
  
  ## record first year and number of years in forcing data frame (may need to overwrite later)
  ndayyear <- 365
  firstyeartrend_forcing <- forcing %>% ungroup() %>% slice(1) %>% pull(date) %>% lubridate::year()
  nyeartrend_forcing <- nrow(forcing)/ndayyear

  ## determine number of seconds per time step
  times <- forcing %>% pull(date) %>% head(2)
  secs_per_tstep <- difftime(times[1], times[2], units = "secs") %>% as.integer() %>% abs()
  
  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::mutate(netrad = -9999.9, fsun = (100-ccov)/100, ndep = 0.0) %>% 
    dplyr::select(temp, rain, vpd, ppfd, netrad, fsun, snow, co2, ndep, fapar, patm)

  ## Tests
  do_continue <- TRUE
  if (makecheck){
    is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
    if (any(is.nanull(forcing$temp))){
      rlang::warn(paste("Error: Missing value in temp for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$rain))){
      rlang::warn(paste("Error: Missing value in rain for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$vpd))){
      rlang::warn(paste("Error: Missing value in vpd for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$ppfd))){
      rlang::warn(paste("Error: Missing value in ppfd for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$netrad))){
      rlang::warn(paste("Error: Missing value in netrad for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$fsun))){
      rlang::warn(paste("Error: Missing value in fsun for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$snow))){
      rlang::warn(paste("Error: Missing value in snow for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$co2))){
      rlang::warn(paste("Error: Missing value in co2 for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$ndep))){
      rlang::warn(paste("Error: Missing value in ndep for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$fapar))){
      rlang::warn(paste("Error: Missing value in fapar for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$patm))){
      rlang::warn(paste("Error: Missing value in patm for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml))


    if (is.nanull(params_siml$spinup)){
      rlang::warn(paste("Error: Missing element in params_siml: spinup"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$spinupyears)){
      rlang::warn(paste("Error: Missing element in params_siml: spinupyears"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$recycle)){
      rlang::warn(paste("Error: Missing element in params_siml: recycle"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$firstyeartrend)){
      rlang::warn(paste("Error: Missing element in params_siml: firstyeartrend"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$nyeartrend)){
      rlang::warn(paste("Error: Missing element in params_siml: nyeartrend"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$soilmstress)){
      rlang::warn(paste("Error: Missing element in params_siml: soilmstress"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$tempstress)){
      rlang::warn(paste("Error: Missing element in params_siml: tempstress"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$calc_aet_fapar_vpd)){
      rlang::warn(paste("Error: Missing element in params_siml: calc_aet_fapar_vpd"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$in_ppfd)){
      rlang::warn(paste("Error: Missing element in params_siml: in_ppfd"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$in_netrad)){
      rlang::warn(paste("Error: Missing element in params_siml: in_netrad"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$outdt)){
      rlang::warn(paste("Error: Missing element in params_siml: outdt"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$ltre)){
      rlang::warn(paste("Error: Missing element in params_siml: ltre"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$ltne)){
      rlang::warn(paste("Error: Missing element in params_siml: ltne"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$ltrd)){
      rlang::warn(paste("Error: Missing element in params_siml: ltrd"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$ltnd)){
      rlang::warn(paste("Error: Missing element in params_siml: ltnd"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$lgr3)){
      rlang::warn(paste("Error: Missing element in params_siml: lgr3"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$lgn3)){
      rlang::warn(paste("Error: Missing element in params_siml: lgn3"))
      do_continue <- FALSE
    }
    if (is.nanull(params_siml$lgr4)){
      rlang::warn(paste("Error: Missing element in params_siml: lgr4"))
      do_continue <- FALSE
    }

    if (nrow(forcing) != params_siml$nyeartrend * ndayyear){
      ## Dates in 'forcing' do not correspond to simulation parameters
      rlang::warn("Error: Number of years data in forcing does not correspond to number of simulation years (nyeartrend).\n")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/ndayyear), "\n")
      rlang::warn(paste(" Number of simulation years: ", params_siml$nyeartrend, "\n"))
      rlang::warn(paste(" Site name: ", sitename, "\n"))
      
      if (nrow(forcing) %% ndayyear == 0){
        ## Overwrite params_siml$nyeartrend and params_siml$firstyeartrend based on 'forcing'
        params_siml$nyeartrend <- nyeartrend_forcing
        params_siml$firstyeartrend <- firstyeartrend_forcing
        rlang::warn(paste(" Overwriting params_siml$nyeartrend: ", params_siml$nyeartrend, "\n"))
        rlang::warn(paste(" Overwriting params_siml$firstyeartrend: ", params_siml$firstyeartrend, "\n"))
      } else {
        ## something weird more fundamentally -> don't run the model
        rlang::warn(" Returning a dummy data frame.")
        do_continue <- FALSE
      }
      
    }
  }

  if (do_continue){

    forcing <- as.matrix(forcing)
    
    n <- as.integer(nrow(forcing))

    ## Model parameters as vector
    par = c(
      as.numeric(params_modl$kphio),
      as.numeric(params_modl$soilm_par_a),
      as.numeric(params_modl$soilm_par_b),
      as.numeric(params_modl$vpdstress_par_a),
      as.numeric(params_modl$vpdstress_par_b),
      as.numeric(params_modl$vpdstress_par_m)
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
      calc_aet_fapar_vpd        = as.logical(params_siml$calc_aet_fapar_vpd),
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
    
    ## Prepare output to be a nice looking tidy data frame (tibble)
    ddf <- init_dates_dataframe(yrstart = params_siml$firstyeartrend, yrend = params_siml$firstyeartrend + params_siml$nyeartrend - 1, noleap = TRUE)

    out <- out %>%
      as.matrix() %>% 
      as.data.frame() %>% 
      setNames(c("fapar", "gpp", "transp", "latenth", "pet", "vcmax", "jmax", "vcmax25", "jmax25", "gs_accl", "wscal", "chi", "iwue")) %>%
      as_tibble(.name_repair = "check_unique") %>%
      # dplyr::mutate(sitename = sitename) %>% 
      dplyr::bind_cols(ddf,.)

  } else {
    out <- tibble(date = lubridate::ymd("2000-01-01"), fapar = NA, gpp = NA, transp = NA, latenth = NA, 
                  pet = NA, vcmax = NA, jmax = NA, vcmax25 = NA, jmax25 = NA, gs_accl = NA, wscal = NA, chi = NA, iwue = NA)    # sitename = sitename, 
  }
    
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
