#' Run P-model (time series)
#' 
#' Run P-model on a single site for a forcing time series.
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
#' @import lubridate
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
#'       (Vcmax) (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{jmax}}{Maximum rate of electron transport for RuBP regeneration
#'       (in mol CO\eqn{_2} m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{vcmax25}}{Maximum rate of carboxylation (Vcmax), 
#'       normalised to 25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).} 
#'   \item{\code{jmax25}}{Maximum rate of electron transport, normalised to 
#'       25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{gs_accl}}{Acclimated stomatal conductance (in 
#'       mol C (mol photons)\eqn{^{-1}} Pa\eqn{^{-1}}. (Multiply by 
#'       ppfd (mol photons m\eqn{^{-2}} d\eqn{^{-1}}) and fapar 
#'       to express per unit ground area and time.)}
#'   \item{\code{wscal}}{Relative soil water content, between 0 (permanent wilting 
#'       point, PWP) and 1 (field capacity, FC).}
#'   \item{\code{chi}}{Ratio of leaf-internal to ambient CO\eqn{_{2}}, ci:ca (unitless).}
#'   \item{\code{iwue}}{Intrinsic water use efficiency (iWUE) (unitless, 
#'       multiply with patm (Pa) to get iWUE in Pa).}
#'   \item{\code{rd}}{Dark respiration (Rd) in gC m\eqn{^{-2}} s\eqn{^{-1}}. 
#'       (Multiply by 1/12 (mol C / gC) to convert to mol C m\eqn{^{-2}} s\eqn{^{-1}}.)}
#'   \item{\code{tsoil}}{Soil temperature, in \eqn{^{o}}C.}
#'   \item{\code{netrad}}{Net radiation, in W m\eqn{^{-2}}. WARNING: this is currently ignored as a model forcing. Instead, net radiation is internally calculated by SPLASH.}
#'   \item{\code{wcont}}{Soil water content, in mm.}
#'   \item{\code{snow}}{Snow water equivalents, in mm.}
#'   \item{\code{cond}}{Water input by condensation, in mm d\eqn{^{-1}}}
#'   \item{\code{cleaf}}{C mass of a virtual leaf carbon pool to keep track of isotopic composition, in gC m\eqn{^{-2}}}
#'   \item{\code{cleafd13c}}{13C isotopic signature (delta) of \code{cleaf}, in permil.}
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
  ccov <- fsun <- NULL
  
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

  # Default value for tc_home
  if ("tc_home" %in% names(site_info)) {
    stop("Unexpectedly received site_info$tc_home; it should be calculated internally.")
  }

  conditionally_add_tmax <- function(df){
    need_to_add_tmax <- !("tmax" %in% colnames(df))
    if (need_to_add_tmax) {
      df %>% dplyr::group_by(.data$date) %>%
        dplyr::summarise(daily_tmax = max(.data$temp, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else {
      df %>% dplyr::rename(daily_tmax = "tmax")
    }
  }
  tc_home <- forcing %>%
    # conditionally add daily max temp (if needed, e.g. when running "gs_leuning" with hourly forcing)
    conditionally_add_tmax() %>%
    # add grouping variables:
    mutate(month = lubridate::month(.data$date), year = lubridate::year(.data$date)) %>%
    # monthly means of daily maximum:
    group_by(.data$year, .data$month) %>%
    summarise(monthly_avg_daily_tmax = mean(.data$daily_tmax, na.rm = TRUE), .groups = "drop") %>%
    # warmest month of each year:
    group_by(year) %>%
    summarise(t_warmest_month = max(.data$monthly_avg_daily_tmax)) %>%
    # mean of yearly warmest months:
    ungroup() %>%
    summarise(tc_home = mean(.data$t_warmest_month, na.rm = TRUE)) %>%
    # extract scalar value
    dplyr::pull(.data$tc_home)

    site_info$tc_home <- tc_home
  # Validate calculation
  if (is.na(site_info$tc_home) || length(site_info$tc_home) == 0) {
    warning("Calculated tc_home is NA or missing; defaulting to 25C.")
    site_info$tc_home <- 25
  }

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
      'tmax',
      'd13c_atm'
  )
  forcing <- forcing %>% 
    dplyr::mutate(fsun = (100-ccov)/100) %>% 
    dplyr::mutate(d13c_atm = -8.4) %>% # xxx demo: hold constant. should be provided by user
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
    
    if (nrow(forcing) %% ndayyear != 0){
      # something weird more fundamentally -> don't run the model
      warning(" Returning a dummy data frame. Forcing data does not
              correspond to full years.")
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
    
    parameter_integrity <- lapply(check_param, function(check_par){
      if (any(is.nanull(params_siml[check_par]))){
        warning(sprintf("Error: Missing value in %s for %s",
                        check_par, sitename))
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    if (suppressWarnings(!all(parameter_integrity))){
      continue <- FALSE
    }

    # model parameters to check
    if ( sum( names(params_modl) %in% c('kphio', 'kphio_par_a', 'kphio_par_b',
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
    
    # in_netrad <- ifelse(any(is.na(forcing$netrad)), FALSE, TRUE)  
    in_netrad <- FALSE  # net radiation is currently ignored as a model forcing, but is internally simulated by SPLASH.
    
    # Check if fsun is available in the case we do NOT provide ppfd
    if (! (in_ppfd)){
      # fsun must be available when ppfd is missing # TODO: actually also with ppfd provided, it must be available for computation of tau and rnl inside of solar() (waterbal_splash.mod.f90:201/213)
      if (any(is.na(forcing$fsun))) continue <- FALSE
    }
  }
  
  if (continue){
    ## C wrapper call
    pmodelout <- .Call(
      'pmodel_f_C',
      secs_per_tstep            = as.integer(secs_per_tstep),
      in_ppfd                   = as.logical(in_ppfd),
      in_netrad                 = as.logical(in_netrad),
      ## Simulation parameters
      spinup                    = as.logical(params_siml$spinup),
      spinupyears               = as.integer(params_siml$spinupyears),
      recycle                   = as.integer(params_siml$recycle),
      firstyeartrend            = as.integer(params_siml$firstyeartrend),
      nyeartrend                = as.integer(params_siml$nyeartrend),
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
      tc_home                   = as.numeric(site_info$tc_home),
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
  } else {
    pmodelout <- NA_real_
  }

  out <- build_out_pmodel(pmodelout, params_siml$firstyeartrend, params_siml$nyeartrend)

  return(out)
}


# Build R output
build_out_pmodel <- function(pmodelout, firstyeartrend, nyeartrend){
  # predefine variables for CRAN check compliance
  . <- NULL

  # Prepare output to be a nice looking tidy data frame (tibble)
  ddf <- init_dates_dataframe(
    yrstart = firstyeartrend,
    yrend = firstyeartrend + nyeartrend - 1,
    noleap = TRUE)

  # create NA output if continue == FALSE (ensure 21 corresponds to column names below)
  if (all(is.na(pmodelout))) { pmodelout <- array(dim = c(1,21), data = NA_real_) }
  
  # Prepare output
  out <- pmodelout %>%
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
        "cond",
        "cleaf",
        "cleafd13c"
        )
    ) %>%
    as_tibble(.name_repair = "check_unique") %>%
    dplyr::bind_cols(ddf, .)
  
  if (all(is.na(pmodelout))){
    # return single row output
    out <- out[1,]
    out$date <- as.Date("2000-01-01")
    out$year_dec <- 2000.000
  } else {
    # return full output
  }
  
  return(out)
}

#' Initialises a tibble with dates
#'
#' Creates a tibble with rows for each date from \code{'yrstart'} to \code{'yrend'}
#' in \code{'yyyy-mm-dd'} format. Intervals of dates are specified by argument 
#'\code{'freq'}. 
#'  ddf <- init_dates_dataframe(2000, 2003, startmoy=1, startdoy=1,
#'                              freq="days", endmoy=12, enddom=31, noleap=FALSE)
#'
#' @param yrstart An integer defining the start year
#'  of dates covered by the dataframe.
#' @param yrend An integer defining the end year of dates
#'  covered by the dataframe.
#' @param startmoy An integer defining the start month-of-year of dates
#'  covered by the dataframe. Defaults to 1.
#' @param startdoy An integer defining the start day-of-year of
#'  dates covered by the dataframe. Defaults to 1.
#' @param freq A character string specifying the time steps of dates
#'  (in rows). Defaults to \code{"days"}. Any of \code{"days", "months", "years"}. If
#'  \code{freq = "months"} the 15\eqn{^{th}} day of the months is used as date,
#'  and if \code{freq = "years"} the 1\eqn{^{st}} of January of each year is returned.
#' @param endmoy An integer defining the end month-of-year of dates covered
#'  by the dataframe. Defaults to 12.
#' @param enddom An integer defining the end day-of-year of dates
#'  covered by the dataframe. Defaults to 31.
#' @param noleap Whether leap years are ignored, that is, whether the 29\eqn{^{th}} 
#' of February is removed. Defaults to \code{FALSE}.
#' 
#' @return A tibble with dates.
#'

init_dates_dataframe <- function(
    yrstart,
    yrend,
    startmoy=1,
    startdoy=1,
    freq="days",
    endmoy=12,
    enddom=31,
    noleap=FALSE ){
  
  if (freq=="days"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-01",
              yrstart, startmoy)) + (startdoy - 1)
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-%02d",
              yrend, endmoy, enddom))
    
  } else if (freq=="months"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-15",
              yrstart, startmoy))
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-15",
              yrend, endmoy))
    
  } else if (freq=="years"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-01",
              yrstart, 1))
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-01",
              yrend, 7))    
  }
  
  # define date range
  date_range <- data.frame(
    date = seq.Date(
      from = start_date,
      to = end_date,
      by = freq
    ))
  
  # convert to decimal date
  numeric_year <- function(x){
    y <- as.numeric(format(x, format="%Y"))
    doy <- as.numeric(format(x, format="%j")) - 1
    
    ifelse(y %% 4 == 0, 
          round(y + doy/366, 3), 
          round(y + doy/365, 3)
    )
  }
  date_range$year_dec <- numeric_year(date_range$date)
  
  # leap year filter
  if (noleap) {
    date_range <- dplyr::filter(date_range,
                                !(format(date, "%m-%d") == "02-29")
    )
    
  }
  
  return(date_range)
}


.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
