#' Run BiomeE
#'
#' Runs the BiomeE model and loads output in once.
#'
#' @param drivers A nested data frame with one row for each site and columns
#'  named according to the arguments of function `runread_biomee_f_bysite()`
#' @param makecheck A logical specifying whether checks are performed to verify
#'  forcings.
#' @param parallel A logical specifying whether simulations are to be 
#'  parallelised (sending data from a certain number of sites to each core). 
#'  Defaults to \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel 
#' computing. Defaults to 2.
#'
#' @return A tibble with one row for each site and model outputs stored 
#' in the nested column \code{data}. See `run_biomee_f_bysite()` for a
#' description of the BiomeE output variables.
#' @export
#' 
#' @examples 
#' \donttest{
#' # Example BiomeE model run
#' 
#' mod_output <- runread_biomee_f(
#'   drivers = biomee_gs_leuning_drivers
#' )
#' }

runread_biomee_f <- function(
  drivers,
  makecheck = TRUE,
  parallel = FALSE,
  ncores = 2
  ){
  
  # predefine variables for CRAN check compliance
  forcing <- init_cohort <- init_soil <- data <-
    input <- params_siml <- params_soil <- params_species <-
    params_tile <- site_info <- sitename <- . <- NULL
  
  if (parallel){
    
    cl <- multidplyr::new_cluster(ncores) %>% 
      multidplyr::cluster_assign(makecheck = FALSE) %>% 
      multidplyr::cluster_library(
        packages = c("dplyr", "purrr", "rsofun")
        )
    
    ## distribute to to cores, making sure all data from a specific site is sent to the same core
    df_out <- drivers %>%
      dplyr::group_by( id = row_number() ) %>%
      tidyr::nest('input' = c("sitename",
                            "params_siml",
                            "site_info",
                            "forcing",
                            "params_tile",
                            "params_species",
                            "params_soil",
                            "init_cohort",
                            "init_soil")) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate('data' = purrr::map(input,
         ~run_biomee_f_bysite(
           sitename       = .x$sitename[[1]], 
           params_siml    = .x$params_siml[[1]], 
           site_info      = .x$site_info[[1]], 
           forcing        = .x$forcing[[1]], 
           params_tile    = .x$params_tile[[1]], 
           params_species = .x$params_species[[1]], 
           params_soil    = .x$params_soil[[1]], 
           init_cohort    = .x$init_cohort[[1]], 
           init_soil      = .x$init_soil[[1]], 
           makecheck      = makecheck )
         
      )) %>% 
      dplyr::collect() %>%
      dplyr::ungroup() %>%
      dplyr::select('data')  %>% 
      tidyr::unnest(cols = c('data'))
    
  } else {
    
    df_out <- drivers %>% 
      dplyr::select("sitename", 
                    "params_siml", 
                    "site_info", 
                    "forcing", 
                    "params_tile", 
                    "params_species", 
                    "params_soil", 
                    "init_cohort", 
                    "init_soil"
                    ) %>% 
      dplyr::mutate(data = purrr::pmap(
        .,
        run_biomee_f_bysite,
        makecheck = makecheck
      )) %>% 
      dplyr::select(sitename, data) 
    
  }
  
  return(df_out)
}
