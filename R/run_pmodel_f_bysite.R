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
      tmax,
      fharv,
      dno3,
      dnh4
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
      "tmax",
      "fharv",
      "dno3",
      "dnh4"
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
      as.numeric(params_modl$f_nretain),
      as.numeric(params_modl$fpc_tree_max),
      as.numeric(params_modl$growtheff),
      as.numeric(params_modl$r_root),
      as.numeric(params_modl$r_sapw),
      as.numeric(params_modl$exurate),
      as.numeric(params_modl$cton_soil),
      as.numeric(params_modl$k_decay_leaf_base),
      as.numeric(params_modl$k_decay_leaf_width),
      as.numeric(params_modl$k_decay_root),
      as.numeric(params_modl$k_decay_labl),
      as.numeric(params_modl$k_decay_sapw),
      as.numeric(params_modl$r_cton_root),
      as.numeric(params_modl$r_cton_wood),
      as.numeric(params_modl$ncw_min),
      as.numeric(params_modl$r_n_cw_v),
      as.numeric(params_modl$r_ctostructn_leaf),
      as.numeric(params_modl$kbeer),
      as.numeric(params_modl$gddbase),
      as.numeric(params_modl$ramp),
      as.numeric(params_modl$phentype),
      as.numeric(params_modl$perc_k1),
      as.numeric(params_modl$thdiff_wp),
      as.numeric(params_modl$thdiff_whc15),
      as.numeric(params_modl$thdiff_fc),
      as.numeric(params_modl$forg),
      as.numeric(params_modl$wbwp),
      as.numeric(params_modl$por),
      as.numeric(params_modl$fsand),
      as.numeric(params_modl$fclay),
      as.numeric(params_modl$fsilt),
      as.numeric(params_modl$kA),
      as.numeric(params_modl$kalb_sw),
      as.numeric(params_modl$kalb_vis),
      as.numeric(params_modl$kb),
      as.numeric(params_modl$kc),
      as.numeric(params_modl$kCw),
      as.numeric(params_modl$kd),
      as.numeric(params_modl$ke),
      as.numeric(params_modl$keps),
      as.numeric(params_modl$kWm),
      as.numeric(params_modl$kw),
      as.numeric(params_modl$komega),
      as.numeric(params_modl$maxmeltrate),
      as.numeric(params_modl$klitt_af10),
      as.numeric(params_modl$klitt_as10),
      as.numeric(params_modl$klitt_bg10),
      as.numeric(params_modl$kexu10),
      as.numeric(params_modl$ksoil_fs10),
      as.numeric(params_modl$ksoil_sl10),
      as.numeric(params_modl$ntoc_crit1),
      as.numeric(params_modl$ntoc_crit2),
      as.numeric(params_modl$cton_microb),
      as.numeric(params_modl$tmppar),
      as.numeric(params_modl$fastfrac),
      as.numeric(params_modl$eff_nup),
      as.numeric(params_modl$minimumcostfix),
      as.numeric(params_modl$fixoptimum),
      as.numeric(params_modl$a_param_fix),
      as.numeric(params_modl$b_param_fix),
      as.numeric(params_modl$maxnitr),
      as.numeric(params_modl$non),
      as.numeric(params_modl$n2on),
      as.numeric(params_modl$kn),
      as.numeric(params_modl$kdoc),
      as.numeric(params_modl$docmax),
      as.numeric(params_modl$dnitr2n2o),

      # additional ones
      as.numeric(params_modl$beta),
      as.numeric(params_modl$rd_to_vcmax),
      as.numeric(params_modl$tau_acclim)
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
        c(
          "fapar",
          "gpp",
          "transp",
          "latenth",
          "pet",
          "vcmax",
          "jmax",
          "vcmax25",
          "jmax25",
          "gs_accl",
          "wscal",
          "chi",
          "iwue",
          "tsoil",
          "lai",
          "cleaf",
          "nleaf",
          "croot",
          "nroot",
          "clabl",
          "nlabl",
          "ninorg",
          "pnh4",
          "pno3",
          "enleach",
          "en2o",
          "tmp",
          "csoil",
          "nsoil",
          "clitt",
          "nlitt",
          "nfix",
          "nup",
          "cex",
          "netmin",
          "dcharv",
          "dnharv"
          )) %>%
      as_tibble(.name_repair = "check_unique") %>%
      dplyr::bind_cols(ddf,.)

  } else {
    out <- tibble(date = lubridate::ymd("2000-01-01"),
                  fapar   = NA,
                  gpp     = NA,
                  transp  = NA,
                  latenth = NA,
                  pet     = NA,
                  vcmax   = NA,
                  jmax    = NA,
                  vcmax25 = NA,
                  jmax25  = NA,
                  gs_accl = NA,
                  wscal   = NA,
                  chi     = NA,
                  iwue    = NA,
                  tsoil   = NA,
                  lai     = NA,
                  cleaf   = NA,
                  nleaf   = NA,
                  croot   = NA,
                  nroot   = NA,
                  clabl   = NA,
                  nlabl   = NA,
                  ninorg  = NA,
                  pnh4    = NA,
                  pno3    = NA,
                  enleach = NA,
                  en2o    = NA,
                  tmp     = NA,
                  csoil   = NA,
                  nsoil   = NA,
                  clitt   = NA,
                  nlitt   = NA,
                  nfix    = NA,
                  nup     = NA,
                  cex     = NA,
                  netmin  = NA,
                  dcharv  = NA,
                  dnharv  = NA
                  )
  }
    
  return(out)

}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
