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
run_sofun_f_bysite <- function( sitename, params_siml, siteinfo, forcing, df_soiltexture, params_modl, makecheck = TRUE ){

  ## re-define units and naming of forcing dataframe
  forcing <- forcing %>% 
    dplyr::mutate(netrad = -9999.9, fsun = (100-ccov)/100, snowf = 0.0, ndep = 0.0) %>% 
    dplyr::select(temp, rainf=prec, vpd, ppfd, netrad, fsun, snowf, co2, ndep, fapar)

  ## Tests
  do_continue <- TRUE
  if (makecheck){
    is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
    if (any(is.nanull(forcing$temp))){
      rlang::warn(paste("Error: Missing value in temp for site", sitename, "\n"))
      do_continue <- FALSE
    }
    if (any(is.nanull(forcing$rainf))){
      rlang::warn(paste("Error: Missing value in rainf for site", sitename, "\n"))
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
    if (any(is.nanull(forcing$snowf))){
      rlang::warn(paste("Error: Missing value in snowf for site", sitename, "\n"))
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
    
    if (nrow(forcing) != params_siml$nyeartrend * 365){
      rlang::warn("Error: Number of years data in forcing does not correspond to number of simulation years (nyeartrend).")
      rlang::warn(paste(" Number of years data: ", nrow(forcing)/365))
      rlang::warn(paste(" Number of simulation years: ", params_siml$nyeartrend))
      rlang::warn(" Returning a dummy data frame.")
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

      'sofun_f_C',
      
      ## Simulation parameters
      spinup                    = as.logical(params_siml$spinup),
      spinupyears               = as.integer(params_siml$spinupyears),
      recycle                   = as.integer(params_siml$recycle),
      firstyeartrend            = as.integer(params_siml$firstyeartrend),
      nyeartrend                = as.integer(params_siml$nyeartrend),
      soilmstress               = as.logical(params_siml$soilmstress),
      tempstress                = as.logical(params_siml$tempstress),
      in_ppfd                   = as.logical(params_siml$in_ppfd),
      in_netrad                 = as.logical(params_siml$in_netrad),
      const_clim_year           = as.integer(params_siml$const_clim_year),
      const_lu_year             = as.integer(params_siml$const_lu_year),
      const_co2_year            = as.integer(params_siml$const_co2_year),
      const_ndep_year           = as.integer(params_siml$const_ndep_year),
      const_nfert_year          = as.integer(params_siml$const_nfert_year),
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
    ddf <- init_dates_dataframe(yrstart = params_siml$firstyeartrend, yrend = siteinfo$year_end, noleap = TRUE)

    out <- out %>%
      as.matrix() %>% 
      as_tibble() %>%
      setNames(c("fapar", "gpp", "transp", "latenth", "XXX")) %>%
      dplyr::mutate(sitename = sitename) %>% 
      dplyr::bind_cols(ddf,.) %>% 
      dplyr::select(-year_dec)

  } else {
    out <- tibble(sitename = sitename, date = NA, fapar = NA, gpp = NA, transp = NA, latenth = NA, XXX = NA)
  }
    
  return(out)

}


.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
