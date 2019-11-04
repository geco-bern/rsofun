#' R wrapper for SOFUN
#'
#' @param settings_sims simulation settings (including simulation parameters parameters)
#' @param params_modl Model parameters 
#' @param list_soiltexture A list of soil texture parameters
#' @param forcing forcing (input) dataframe (returned object by \link{prepare_input_sofun})
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#'
#' @details This is the model
#'
#' @export
#' @useDynLib rsofun
#'
run_sofun_f_bysite <- function( settings_sims, params_modl, list_soiltexture, forcing, makecheck = TRUE ){

  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::mutate(netrad = -9999.9, fsun = (100-ccov)/100, snowf = 0.0, ndep = 0.0) %>% 
    dplyr::select(temp, rainf=prec, vpd, ppfd, netrad, fsun, snowf, co2, ndep, fapar)

  ## Tests
  if (makecheck){
    is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
    cont <- TRUE
    if (any(is.nanull(forcing$temp))){
      rlang::warn(paste("Error: Missing value in temp for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$rainf))){
      rlang::warn(paste("Error: Missing value in rainf for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$vpd))){
      rlang::warn(paste("Error: Missing value in vpd for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$ppfd))){
      rlang::warn(paste("Error: Missing value in ppfd for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$netrad))){
      rlang::warn(paste("Error: Missing value in netrad for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$fsun))){
      rlang::warn(paste("Error: Missing value in fsun for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$snowf))){
      rlang::warn(paste("Error: Missing value in snowf for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$co2))){
      rlang::warn(paste("Error: Missing value in co2 for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$ndep))){
      rlang::warn(paste("Error: Missing value in ndep for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    if (any(is.nanull(forcing$fapar))){
      rlang::warn(paste("Error: Missing value in fapar for site", settings_sims$sitename, "\n"))
      cont <- FALSE
    }
    
    if (nrow(forcing) != settings_sims$params_siml[[1]]$nyeartrend * 365){
      rlang::warn("Error: Number of years data in forcing does not correspond to number of simulation years (nyeartrend).")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/365))
      rlang::warn(paste(" Number of simulation years: ", settings_sims$params_siml[[1]]$nyeartrend))
      rlang::warn(" Returning a dummy data frame.")
    }
  }

  if (cont){
    forcing <- as.matrix(forcing)
    
    n = as.integer(nrow(forcing))

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
    soiltexture = data.frame(
      top =  c(
      as.numeric(list_soiltexture$top$fsand), 
      as.numeric(list_soiltexture$top$fclay), 
      as.numeric(list_soiltexture$top$forg), 
      as.numeric(list_soiltexture$top$fgravel)),
      bottom =  c(
      as.numeric(list_soiltexture$bottom$fsand), 
      as.numeric(list_soiltexture$bottom$fclay), 
      as.numeric(list_soiltexture$bottom$forg), 
      as.numeric(list_soiltexture$bottom$fgravel))
      ) %>% 
      as.matrix()
    
    ## unnest simulation parameters list
    settings_sims <- settings_sims %>% 
      tidyr::unnest(params_siml)

    ## C wrapper call
    out <- .Call(

      'sofun_f_C',
      
      ## Simulation parameters
      spinup                    = as.logical(settings_sims$spinup),
      spinupyears               = as.integer(settings_sims$spinupyears),
      recycle                   = as.integer(settings_sims$recycle),
      firstyeartrend            = as.integer(settings_sims$firstyeartrend),
      nyeartrend                = as.integer(settings_sims$nyeartrend),
      soilmstress               = as.logical(settings_sims$soilmstress),
      tempstress                = as.logical(settings_sims$tempstress),
      in_ppfd                   = as.logical(settings_sims$in_ppfd),
      in_netrad                 = as.logical(settings_sims$in_netrad),
      const_clim_year           = as.integer(settings_sims$const_clim_year),
      const_lu_year             = as.integer(settings_sims$const_lu_year),
      const_co2_year            = as.integer(settings_sims$const_co2_year),
      const_ndep_year           = as.integer(settings_sims$const_ndep_year),
      const_nfert_year          = as.integer(settings_sims$const_nfert_year),
      daily_out_startyr         = as.integer(settings_sims$daily_out_startyr),
      daily_out_endyr           = as.integer(settings_sims$daily_out_endyr),
      outdt                     = as.integer(settings_sims$outdt),
      ltre                      = as.logical(settings_sims$ltre),
      ltne                      = as.logical(settings_sims$ltne),
      ltrd                      = as.logical(settings_sims$ltrd),
      ltnd                      = as.logical(settings_sims$ltnd),
      lgr3                      = as.logical(settings_sims$lgr3),
      lgn3                      = as.logical(settings_sims$lgn3),
      lgr4                      = as.logical(settings_sims$lgr4),
      loutplant                 = as.logical(settings_sims$loutplant),
      loutgpp                   = as.logical(settings_sims$loutgpp),
      loutwaterbal              = as.logical(settings_sims$loutwaterbal),
      loutforcing               = as.logical(settings_sims$loutforcing),
      loutdgpp                  = as.logical(settings_sims$loutdgpp),
      loutdrd                   = as.logical(settings_sims$loutdrd),
      loutdtransp               = as.logical(settings_sims$loutdtransp),
      loutdwcont                = as.logical(settings_sims$loutdwcont),
      loutdaet                  = as.logical(settings_sims$loutdaet),
      loutdpet                  = as.logical(settings_sims$loutdpet),
      loutdnetrad               = as.logical(settings_sims$loutdnetrad),
      loutdwbal                 = as.logical(settings_sims$loutdwbal),
      loutdtemp                 = as.logical(settings_sims$loutdtemp),
      loutdfapar                = as.logical(settings_sims$loutdfapar),
      loutdtemp_soil            = as.logical(settings_sims$loutdtemp_soil),
      lcalibgpp                 = as.logical(settings_sims$lcalibgpp),
      lcalibfapar               = as.logical(settings_sims$lcalibfapar),
      lcalibtransp              = as.logical(settings_sims$lcalibtransp),
      lcaliblatenth             = as.logical(settings_sims$lcaliblatenth),
      longitude                 = as.numeric(settings_sims$lon),
      latitude                  = as.numeric(settings_sims$lat),
      altitude                  = as.numeric(settings_sims$elv),
      whc                       = as.numeric(settings_sims$whc),
      soiltexture               = soiltexture,
      n                         = n,
      par                       = par, 
      forcing                   = forcing
      )
    
    ## Prepare output to be a nice looking tidy data frame (tibble)
    ddf <- init_dates_dataframe(yrstart = settings_sims$firstyeartrend, yrend = settings_sims$year_end, noleap = TRUE)

    out <- out %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("fapar", "gpp", "transp", "latenth", "XXX")) %>%
      dplyr::mutate(sitename = settings_sims$sitename) %>% 
      dplyr::bind_cols(ddf,.) %>% 
      dplyr::select(-year_dec)

  } else {
    out <- tibble(sitename = settings_sims$sitename, date = NA, fapar = NA, gpp = NA, transp = NA, latenth = NA, XXX = NA)
  }
    
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
