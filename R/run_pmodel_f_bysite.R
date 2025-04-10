#' Run P-model (R wrapper)
#' 
#' Run P-model Fortran model on single site.
#'
#' @param sitename Site name.
#' @param params_siml Simulation parameters.
#' @param site_info Site meta info in a data.frame.
#' @param forcing A data frame of forcing climate data, used as input.
#' @param params_modl A named list of free (calibratable) model parameters. See \code{\link{runread_pmodel_f}}
#' @param makecheck A logical specifying whether checks are performed 
#'  to verify forcings and model parameters. \code{TRUE} by default.
#' @param verbose A logical specifying whether to print warnings.
#' Defaults to \code{TRUE}.
#'
#' For further specifications of above inputs and examples see \code{\link{p_model_drivers}} or \code{\link{p_model_drivers_vcmax25}}

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
#'   \item{\code{aet}}{Actual evapotranspiration (AET), calculated by SPLASH following Priestly-Taylor (in mm d\eqn{^{-1}}).}
#'   \item{\code{le}}{Latent heat flux (in J m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{pet}}{Potential evapotranspiration (PET), calculated by SPLASH following Priestly-Taylor (in mm d\eqn{^{-1}}).}
#'   \item{\code{vcmax}}{Maximum rate of RuBisCO carboxylation 
#'       (Vcmax) (in mol C m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{jmax}}{Maximum rate of electron transport for RuBP regeneration
#'       (in mol CO\eqn{_2} m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{vcmax25}}{Maximum rate of carboxylation (Vcmax), 
#'       normalised to 25\eqn{^o}C (in mol C m\eqn{^{-2}} d\eqn{^{-1}}).}
#'   \item{\code{jmax25}}{Maximum rate of electron transport, normalised to 
#'       25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{gs_accl}}{Acclimated stomatal conductance (in 
#'       mol C m\eqn{^{-2}} d\eqn{^{-1}} Pa\eqn{^{-1}}).}
#'   \item{\code{wscal}}{Relative soil water content, between 0 (permanent wilting 
#'       point, PWP) and 1 (field capacity, FC).}
#'   \item{\code{chi}}{Ratio of leaf-internal to ambient CO\eqn{_{2}}, ci:ca (unitless).}
#'   \item{\code{iwue}}{Intrinsic water use efficiency (iWUE) (in Pa).}
#'   \item{\code{rd}}{Dark respiration (Rd) in gC m\eqn{^{-2}} d\eqn{^{-1}}.}
#'   \item{\code{tsoil}}{Soil temperature, in \eqn{^{o}}C.}
#'   \item{\code{netrad}}{Net radiation, in W m\eqn{^{-2}}. WARNING: this is currently ignored as a model forcing. Instead, net radiation is internally calculated by SPLASH.}
#'   \item{\code{wcont}}{Soil water content, in mm.}
#'   \item{\code{snow}}{Snow water equivalents, in mm.}
#'   \item{\code{cond}}{Water input by condensation, in mm d\eqn{^{-1}}}
#'   } 
#'   
#' @details Depending on the input model parameters, it's possible to run the 
#' different P-model setups presented in Stocker et al. 2020 GMD. The P-model
#' version implemented in this package allows more flexibility than the one
#' presented in the paper, with the following functions:
#' 
#' The temperature dependence of the quantum yield efficiency is given by: \cr
#' \eqn{\varphi_0 (T) = c (1 + a (T - b)^2 ) } if \eqn{0 < c (1 + a (T - b)^2 ) < 1}, \cr
#' \eqn{\varphi_0 (T) = 0 } if \eqn{ c (1 + a (T - b)^2 ) \leq 0}, and  \cr
#' \eqn{\varphi_0 (T) = 1 } if \eqn{ c (1 + a (T - b)^2 ) \geq 1}. \cr
#' The ORG setup can be reproduced by setting \code{kphio_par_a = 0}
#' and calibrating the \code{kphio} parameter only.
#' The BRC setup (which calibrates \eqn{c_L = \frac{a_L b_L}{4}} in Eq. 18) is more difficult to reproduce, 
#' since the temperature-dependency has been reformulated and a custom cost
#' function would be necessary for calibration. The new parameters
#' are related to \eqn{c_L} as follows: \cr
#' \eqn{a = -0.0004919819} \cr
#' \eqn{b = 32.35294} \cr
#' \eqn{c = 0.6910823 c_L} 
#' 
#' The soil moisture stress is implemented as \cr
#' \eqn{\beta(\theta) = \frac{\beta_0 - 1}{{\theta^{*}}^2} 
#'    (\theta - \theta^{*})^2 + 1 } if 
#'    \eqn{ 0 \leq \theta \leq \theta^{*}} and \cr
#' \eqn{\beta(\theta) = 1} if \eqn{ \theta > \theta^{*}}. \cr
#' In Stocker et al. 2020 GMD, the threshold plant-available soil water is set as
#' \eqn{\theta^{*}} 
#' \code{= 0.6 * whc} where \code{whc} is the site's water holding capacity. Also,
#' the \eqn{\beta} reduction at low soil moisture (\eqn{\beta_0 = \beta(0)}) was parameterized
#' as a linear function of mean aridity (Eq. 20 in Stocker et al. 2020 GMD) but is
#' considered a constant model parameter in this package. 
#' Hence, the FULL calibration setup cannot be 
#' exactly replicated.
#'
#' @export
#' @useDynLib rsofun
#'
#' @examples
#' # Define model parameter values from previous work
#' params_modl <- list(
#'   kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
#'   kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
#'   kphio_par_b        = 1.0,
#'   soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'   tau_acclim         = 30.0,
#'   kc_jmax            = 0.41
#' )
#' 
#' # Run the Fortran P-model 
#' mod_output <- run_pmodel_f_bysite(
#'   # unnest drivers example data
#'   sitename = p_model_drivers$sitename[1],
#'   params_siml = p_model_drivers$params_siml[[1]],
#'   site_info = p_model_drivers$site_info[[1]],
#'   forcing = p_model_drivers$forcing[[1]],
#'   params_modl = params_modl
#'  )

run_pmodel_f_bysite <- function(
    sitename,
    params_siml,
    site_info,
    forcing,
    params_modl,
    makecheck = TRUE,
    verbose = TRUE
){
  
  # predefine variables for CRAN check compliance
  ccov <- fsun <- . <- NULL
  
  # base state, always execute the call
  continue <- TRUE
  
  # record first year and number of years in forcing data
  # frame (may need to overwrite later) to use as default values
  ndayyear <- 365
  forcing_years <- nrow(forcing)/ndayyear

  firstyear_forcing <- forcing %>%
    dplyr::ungroup() %>%
    dplyr::slice(1) %>%
    dplyr::pull(date) %>%
    format("%Y") %>%
    as.numeric()

  # Default value for nyeartrend
  if ('nyeartrend' %in% names(params_siml)) {stop("Unexpectedly received params_siml$nyeartrend for p-model.")}
  params_siml$nyeartrend <- forcing_years

  # Default value for firstyeartrend
  # For p-model we anchor to the first year of the forcing, meaning that spinup
  # years are before the first year of forcing
  if ('firstyeartrend' %in% names(params_siml)) {stop("Unexpectedly received params_siml$firstyeartrend for p-model.")}
  params_siml$firstyeartrend <- firstyear_forcing
  
  # determine number of seconds per time step
  times <- forcing %>%
    dplyr::pull(date) %>%
    utils::head(2)
  secs_per_tstep <- difftime(times[1], times[2], units = "secs") %>%
    as.integer() %>%
    abs()
  
  # re-define units and naming of forcing dataframe
  # keep the order of columns - it's critical for Fortran (reading by column number)
  forcing_features <- c(
      'temp',
      'rain',
      'vpd',
      'ppfd',
      'netrad',
      'fsun',
      'snow',
      'co2',
      'fapar',
      'patm',
      'tmin',
      'tmax'
  )
  forcing <- forcing %>% 
    dplyr::mutate(fsun = (100-ccov)/100) %>% 
    dplyr::select(
        all_of(forcing_features)
    )
  
  # validate input
  if (makecheck){

    is.nanull <- function(x) ifelse(any(is.null(x), is.na(x)), TRUE, FALSE)
    
    # list variable to check for
    check_vars <- c(
      "temp",
      "rain",
      "vpd",
      # 'ppfd',   # TODO: activate check for these. Use forcing_features instead of check_vars
      # 'netrad', # TODO: activate check for these. Use forcing_features instead of check_vars
      # 'fsun',   # TODO: activate check for these. Use forcing_features instead of check_vars
      "snow",
      "co2",
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
    
    # only run simulation if all checked variables are valid
    # suppress warning on coercion of list to single logical
    if (suppressWarnings(!all(data_integrity))){
      continue <- FALSE
    }
    
    # simulation parameters to check
    check_param <- c(
      "spinup",
      "spinupyears",
      "recycle",
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
    
    if (nrow(forcing) %% ndayyear != 0){
      # something weird more fundamentally -> don't run the model
      warning(" Returning a dummy data frame. Forcing data does not
              correspond to full years.")
      continue <- FALSE
    }
    
    # model parameters to check
    if( sum( names(params_modl) %in% c('kphio', 'kphio_par_a', 'kphio_par_b',
                                       'soilm_thetastar', 'soilm_betao',
                                       'beta_unitcostratio', 'rd_to_vcmax', 
                                       'tau_acclim', 'kc_jmax')
    ) != 9){
      warning(" Returning a dummy data frame. Incorrect model parameters.")
      continue <- FALSE
    }
  }
  
  if (continue){
    
    # determine whether to read PPFD from forcing or to calculate internally
    in_ppfd <- ifelse(any(is.na(forcing$ppfd)), FALSE, TRUE)  
    
    # determine whether to read PPFD from forcing or to calculate internally
    # in_netrad <- ifelse(any(is.na(forcing$netrad)), FALSE, TRUE)  
    in_netrad <- FALSE  # net radiation is currently ignored as a model forcing, but is internally simulated by SPLASH.
    
    # Check if fsun is available
    if(! (in_ppfd & in_netrad)){
      # fsun must be available when one of ppfd or netrad is missing
      if(any(is.na(forcing$fsun))) continue <- FALSE
    }
  }
  
  if(continue){
    
    
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
      in_ppfd                   = as.logical(in_ppfd),
      in_netrad                 = as.logical(in_netrad),
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
      n                         = as.integer(nrow(forcing)), # number of rows in matrix (pre-allocation of memory)
      par                       = c(as.numeric(params_modl$kphio), # model parameters as vector in order
                                    as.numeric(params_modl$kphio_par_a),
                                    as.numeric(params_modl$kphio_par_b),
                                    as.numeric(params_modl$soilm_thetastar),
                                    as.numeric(params_modl$soilm_betao),
                                    as.numeric(params_modl$beta_unitcostratio),
                                    as.numeric(params_modl$rd_to_vcmax),
                                    as.numeric(params_modl$tau_acclim),
                                    as.numeric(params_modl$kc_jmax)),
      forcing                   = as.matrix(forcing)
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
        c("fapar", 
          "gpp", 
          "aet", 
          "le", 
          "pet", 
          "vcmax",
          "jmax", 
          "vcmax25", 
          "jmax25", 
          "gs_accl", 
          "wscal", 
          "chi", 
          "iwue", 
          "rd",
          "tsoil", 
          "netrad", 
          "wcont", 
          "snow",
          "cond")
      ) %>%
      as_tibble(.name_repair = "check_unique") %>%
      dplyr::bind_cols(ddf,.)
    
  } else {
    out <- tibble(date = as.Date("2000-01-01"),
                  fapar = NA, 
                  gpp = NA, 
                  transp = NA, 
                  latenth = NA, 
                  pet = NA, 
                  vcmax = NA, 
                  jmax = NA, 
                  vcmax25 = NA, 
                  jmax25 = NA, 
                  gs_accl = NA, 
                  wscal = NA, 
                  chi = NA, 
                  iwue = NA, 
                  rd = NA, 
                  tsoil = NA, 
                  netrad = NA,
                  wcont = NA, 
                  snow = NA,
                  cond = NA)
  }
  
  return(out)
  
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
