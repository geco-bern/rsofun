#' Run the P-model
#'
#' Runs the P-model and loads output in once.
#'
#' @param drivers A nested data frame with one row for each site and columns
#'  named according to the arguments of function \code{\link{run_pmodel_f_bysite}},
#'  namely \code{sitename, params_siml, site_info} and \code{forcing}.
#' @param par A named list of free (calibratable) model parameters.
#' \describe{
#'   \item{kphio}{The quantum yield efficiency at optimal temperature \eqn{\varphi_0}, 
#'    in mol mol\eqn{^{-1}}.
#'    When temperature dependence is used, it corresponds to the multiplicative
#'    parameter \eqn{c} (see Details).}
#'   \item{kphio_par_a}{The shape parameter \eqn{a} of the temperature-dependency of
#'    quantum yield efficiency (see Details).
#'    To disable the temperature dependence, set \code{kphio_par_a = 0}.}
#'   \item{kphio_par_b}{The optimal temperature parameter \eqn{b} of the temperature
#'    dependent quantum yield efficiency (see Details), in \eqn{^o}C.}
#'   \item{soilm_thetastar}{The threshold parameter \eqn{\theta^{*}} in the 
#'    soil moisture stress function (see Details), given in mm.
#'    To turn off the soil moisture stress, set \code{soilm_thetastar = 0}.}
#'   \item{soilm_betao}{The intercept parameter \eqn{\beta_{0}} in the
#'    soil moisture stress function (see Details). This is the parameter calibrated 
#'    in Stocker et al. 2020 GMD.}
#'   \item{beta_unitcostratio}{The unit cost of carboxylation, corresponding to
#'    \eqn{\beta = b / a'} in Eq. 3 of Stocker et al. 2020 GMD.}
#'   \item{rd_to_vcmax}{Ratio of Rdark (dark respiration) to Vcmax25.}
#'   \item{tau_acclim}{Acclimation time scale of photosynthesis, in days.}
#'   \item{kc_jmax}{Parameter for Jmax cost ratio (corresponding to c\eqn{^*} in
#'   Stocker et al. 2020 GMD).} 
#' }
#' @param makecheck A logical specifying whether checks are performed to verify
#'  forcings. Defaults to \code{TRUE}.
#' @param parallel A logical specifying whether simulations are to be
#'  parallelised (sending data from a certain number of sites to each core).
#'  Defaults to \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#'  computing (by default \code{ncores = 2}).
#'
#' @return A data frame (tibble) with one row for each site, site information 
#' stored in the nested column \code{site_info} and outputs stored in the nested 
#' column \code{data}. See \code{\link{run_pmodel_f_bysite}} for a detailed 
#' description of the outputs.
#' @export
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
#' # Run the model for these parameters and the example drivers
#' output <- rsofun::runread_pmodel_f(
#'   drivers = rsofun::p_model_drivers,
#'   par = params_modl)

runread_pmodel_f <- function(
  drivers,
  par,
  makecheck = TRUE,
  parallel = FALSE,
  ncores = 1){
  
  # predefine variables for CRAN check compliance
  sitename <- params_siml <- site_info <-
    input <- forcing <- . <- NULL
  
  # guarantee order of files
  drivers <- drivers |>
    dplyr::select(
      sitename,
      params_siml,
      site_info,
      forcing
    )
  
  if (parallel){
    
    cl <- multidplyr::new_cluster(n = ncores) |>
      multidplyr::cluster_assign(par = par) |>
      multidplyr::cluster_assign(makecheck = FALSE) |>
      multidplyr::cluster_library(
        packages = c("dplyr", "purrr", "rsofun")
      )
    
    # distribute to to cores, making sure all data from
    # a specific site is sent to the same core
    df_out <- drivers |>
      dplyr::group_by(id = row_number()) |>
      tidyr::nest(
        input = c(
          sitename,
          params_siml,
          site_info,
          forcing)
      ) %>%
      multidplyr::partition(cl) %>% 
      dplyr::mutate(data = purrr::map(input, 
                                      ~run_pmodel_f_bysite(
                                        sitename       = .x$sitename[[1]], 
                                        params_siml    = .x$params_siml[[1]], 
                                        site_info       = .x$site_info[[1]], 
                                        forcing        = .x$forcing[[1]], 
                                        par    = par, 
                                        makecheck      = makecheck )
      ))
    
    # collect the cluster data
    data <- df_out |>
      dplyr::collect() |>
      dplyr::ungroup() |>
      dplyr::select(data)
    
    # meta-data
    meta_data <- df_out |>
      dplyr::collect() |>
      dplyr::ungroup() |>
      dplyr::select( input ) |>
      tidyr::unnest( cols = c( input )) |>
      dplyr::select(sitename, site_info)
    
    # combine both data and meta-data
    # this implicitly assumes that the order
    # between the two functions above does
    # not alter! There is no way of checking
    # in the current setup
    df_out <- bind_cols(meta_data, data)
    
  } else {
    
    # note that pmap() requires the object 'drivers' to have columns in the order
    # corresponding to the order of arguments of run_pmodel_f_bysite().
    df_out <- drivers %>%
      dplyr::mutate(
        data = purrr::pmap(.,
        	run_pmodel_f_bysite,
            params_modl = par,
            makecheck = makecheck
        )
      ) |> 
      dplyr::select(sitename, site_info, data)
  }
  
  return(df_out)
}
