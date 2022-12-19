#' R wrapper for SOFUN P-model
#' 
#' Call to the Fortran P-model
#'
#' @param sitename Site name.
#' @param params_siml Simulation parameters.
#' \describe{
#'       \item{spinup}{A logical value indicating whether this simulation does spin-up.}
#'       \item{spinupyears}{Number of spin-up years.}
#'       \item{recycle}{Length of standard recycling period, in days.}
#'       \item{soilmstress}{A logical value, if \code{TRUE} an empirical soil 
#'       moisture stress function is applied to GPP.}
#'       \item{tempstress}{A logical value, if \code{TRUE} an empirical temperature 
#'       stress function is applied to GPP.}
#'       \item{calc_aet_fapar_vpd}{(not in use)}
#'       \item{in_ppfd}{A logical value, if \code{TRUE} PPFD is a prescribed variable, 
#'       if \code{FALSE} PPFD is simulated internally.}
#'       \item{in_netrad}{A logical value indicating whether net radiation is 
#'       prescribed (\code{TRUE}) or simulated internally (\code{FALSE}).}
#'       \item{outdt}{An integer indicating the output periodicity.}
#'       \item{ltre}{A logical value, \code{TRUE} if evergreen tree.}
#'       \item{ltne}{A logical value, \code{TRUE} if evergreen tree and N-fixing.}
#'       \item{ltrd}{A logical value, \code{TRUE} if deciduous tree.}
#'       \item{ltnd}{A logical value, \code{TRUE} if deciduous tree and N-fixing.}
#'       \item{lgr3}{A logical value, \code{TRUE} if grass with C3 photosynthetic pathway.}
#'       \item{lgn3}{A logical value, \code{TRUE} if grass with C3 photosynthetic
#'       pathway and N-fixing.}
#'       \item{lgr4}{A logical value, \code{TRUE} if grass with C4 photosynthetic pathway.}
#'       \item{firstyeartrend}{The year AD of first transient year.}
#'       \item{nyeartrend}{The number of transient years.}
#' }
#' @param site_info A list of site meta info. Required:
#' \describe{
#'       \item{lon}{Longitud of the site location.}
#'       \item{lat}{Latitude of the site location.}
#'       \item{elv}{Elevation of the site location, in meters.}
#'       \item{whc}{A numeric value for the water holding capacity (in mm), used for 
#'       simulating the soil water balance.}
#' }
#' @param forcing A data frame of forcing climate data, used as input 
#'  (returned object by \code{\link{collect_drivers_sofun}}, see \code{\link{p_model_drivers}}
#'  for a detailed description of its structure and contents).
#' @param params_soil A list of soil texture parameters, for the top and bottom
#' layer of soil.
#' \describe{
#'       \item{fsand}{The fraction of sand in the soil.}
#'       \item{fclay}{The fraction of clay in the soil.}
#'       \item{forg}{The fraction of organic matter in the soil.}
#'       \item{fgravel}{The fraction of gravel in the soil.}
#' }
#' @param params_modl A named list of free (calibratable) model parameters.
#' \describe{
#'   \item{kphio}{The quantum yield efficiency parameter, in mol mol\eqn{^{-1}}.}
#'   \item{soilm_par_a}{Intercept of the linear soil moisture stress function (unitless).}
#'   \item{soilm_par_b}{Slope of the linear soil moisture stress function (unitless).}
#'   \item{tau_acclim_tempstress}{Memory e-folding time scale parameter (in hours)
#'   for acclimation of photosynthesis, following Eq. 5 in Makela et al. (2004) 
#'   Tree Physiology 24, 369–376}
#'   \item{par_shape_tempstress}{Shape parameter for the low temperature stress
#'   function affecting quantum yield efficiency. A positive (unitless) value indicating 
#'   the sensitivity of the decline (assuming no decline at 10\eqn{^o}C) and 
#'   above, but decreasing with a quadratic function below 10\eqn{^o}C).}
#' }
#' @param makecheck A logical specifying whether checks are performed 
#'  to verify forcings and model parameters. \code{TRUE} by default.
#' @param verbose A logical specifying whether to print warnings.
#' Defaults to \code{TRUE}.
#'
#' @import dplyr
#' 
#' @returns Model output is provided as a tidy dataframe, with columns:
#' \describe{
#'   \item{\code{date}}{Date of the observation in YYYY-MM-DD format.}
#'   \item{\code{year_dec}}{Decimal representation of year and day of the year
#'      (for example, 2007.000 corresponds to 2007-01-01 and 2007.003 to 2007-01-02.}
#'   \item{\code{fapar}}{Fraction of photosynthetic active radiation (fAPAR), taking
#'      values between 0 and 1.}
#'   \item{\code{gpp}}{Gross Primary Productivity (GPP) for each time stamp 
#'       (in gC m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{transp}}{Actual evapotranspiration (AET) (in mm d\eqn{^{-1}}).}
#'   \item{\code{latenth}}{Latent heat flux (in J m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{pet}}{Potential evapotranspiration (PET) (in mm d\eqn{^{-1}}).}
#'   \item{\code{vcmax}}{Maximum rate of RuBisCO carboxylation 
#'       (Vcmax) (in mol C m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{jmax}}{Maximum rate of electron transport for RuBP regeneration
#'       (in mol CO\eqn{_2} m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{vcmax25}}{Maximum rate of carboxylation (Vcmax), 
#'       normalised to 25\eqn{^o}C (in mol C m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{jmax25}}{Maximum rate of electron transport, normalised to 
#'       25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{gs_accl}}{Acclimated stomatal conductance (in 
#'       mol C m\eqn{^{−2}} d\eqn{^{−1}} Pa\eqn{^{−1}}).}
#'   \item{\code{wscal}}{Relative soil water content, between 0 (permanent wilting 
#'       point, PWP) and 1 (field capacity, FC).}
#'   \item{\code{chi}}{Ratio of leaf-internal to ambient CO\eqn{_{2}}, ci:ca (unitless).}
#'   \item{\code{iwue}}{Intrinsic water use efficiency (iWUE) (in Pa).}
#'   \item{\code{rd}}{Dark respiration (Rd) in gC m\eqn{^{-2}} d\eqn{^{-1}}.}
#'   } 
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
    
    # Check model parameters
    if(all(!is.nanull(params_siml$soilmstress), !is.nanull(params_siml$tempstress))){
      if (length(params_modl) < 1){
        warning("Error: Missing model parameters")
        continue <- FALSE
      } else if (is.nanull(params_modl$kphio)){
        warning("Error: Missing kphio parameter, cannot run model.")
        continue <- FALSE
      }
      if (params_siml$soilmstress){
        if (any(is.nanull(params_modl$soilm_par_a), 
                is.nanull(params_modl$soilm_par_b))){
          warning("Error: Missing soilm_par_a and soilm_par_b parameters but soilmstress = TRUE.")
          continue <- FALSE
        } 
      }
      if(params_siml$tempstress){
        if (any(is.nanull(params_modl$tau_acclim_tempstress),
                is.nanull(params_modl$par_shape_tempstress))){
          warning("Error: Missing tau and shape parameters but tempstress = TRUE.")
          continue <- FALSE
        }
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
      as.numeric(params_modl$par_shape_tempstress)
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
          "jmax", "vcmax25", "jmax25", "gs_accl", "wscal", "chi", "iwue", "rd",
          "tsoil")
        ) %>%
      as_tibble(.name_repair = "check_unique") %>%
      dplyr::bind_cols(ddf,.)

  } else {
    out <- tibble(date = lubridate::ymd("2000-01-01"),
                  fapar = NA, gpp = NA, transp = NA, latenth = NA, 
                  pet = NA, vcmax = NA, jmax = NA, vcmax25 = NA, 
                  jmax25 = NA, gs_accl = NA, wscal = NA, chi = NA, 
                  iwue = NA, rd = NA, tsoil = NA)
  }
    
  return(out)

}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
