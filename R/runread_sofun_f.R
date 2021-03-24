#' Run the P-model
#'
#' Runs the P-model and loads output in once.
#'
#' @param df_drivers A nested data frame with one row for each site and columns named according to the 
#' arguments of function \link{runread_pmodel_f_bysite}
#' @param params_modl A named list of model parameters
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#' @param parallel A logical specifying whether simulations are to be parallelised (sending 
#' data from a certain number of sites to each core). Defaults to \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel computing. 
#' Defaults to 2.
#'
#' @return A data frame (tibble) with one row for each site and outputs stored in the nested
#' column \code{data}
#' @export
#'
#' @examples mod <- runread_pmodel_f( df_drivers, params_modl, makecheck = TRUE, parallel = FALSE, ncores = 2 )
#' 
runread_pmodel_f <- function( df_drivers, params_modl, makecheck = TRUE, parallel = FALSE, ncores = 2 ){

  if (parallel){

    cl <- multidplyr::new_cluster(ncores) %>% 
      multidplyr::cluster_assign(params_modl = params_modl) %>% 
      multidplyr::cluster_assign(makecheck = FALSE) %>% 
      multidplyr::cluster_library(c("dplyr", "purrr", "rlang", "rsofun"))
    
    ## distribute to to cores, making sure all data from a specific site is sent to the same core
    df_out <- df_drivers %>%
      dplyr::group_by( id = row_number() ) %>%
      tidyr::nest(input = c(sitename, params_siml, siteinfo, forcing, df_soiltexture)) %>%
      multidplyr::partition(cl) %>% 
      dplyr::mutate(data = purrr::map( input, 
                                     ~run_pmodel_f_bysite(
                                       sitename       = .x$sitename[[1]], 
                                       params_siml    = .x$params_siml[[1]], 
                                       siteinfo       = .x$siteinfo[[1]], 
                                       forcing        = .x$forcing[[1]], 
                                       df_soiltexture = .x$df_soiltexture[[1]], 
                                       params_modl    = params_modl, 
                                       makecheck      = makecheck )
      )) %>% 
      dplyr::collect() %>%
      dplyr::ungroup() %>%
      dplyr::select( data )  %>% 
      tidyr::unnest( cols = c( data ))
    
  } else {
  
    df_out <- df_drivers %>% 
      dplyr::mutate(data = purrr::pmap(
        .,
        run_pmodel_f_bysite,
        params_modl = params_modl,
        makecheck = makecheck
      )) %>% 
      dplyr::select(sitename, data)
    
  }
  
  return(df_out)
}

#' Run LM3-PPA
#'
#' Runs the LM3-PPA model and loads output in once.
#'
#' @param df_drivers A nested data frame with one row for each site and columns named according to the 
#' arguments of function \link{runread_pmodel_f_bysite}
#' @param params_modl A named list of model parameters
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#' @param parallel A logical specifying whether simulations are to be parallelised (sending 
#' data from a certain number of sites to each core). Defaults to \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel computing. 
#' Defaults to 2.
#'
#' @return A data frame (tibble) with one row for each site and outputs stored in the nested
#' column \code{data}
#' @export
#'
#' @examples mod <- runread_pmodel_f( df_drivers, params_modl, makecheck = TRUE, parallel = FALSE, ncores = 2 )
#' 
runread_lm3ppa_f <- function( df_drivers, makecheck = TRUE, parallel = FALSE, ncores = 2 ){
  
  if (parallel){
    
    cl <- multidplyr::new_cluster(2) %>% 
      # multidplyr::cluster_assign(params_modl = params_modl) %>% 
      multidplyr::cluster_assign(makecheck = FALSE) %>% 
      multidplyr::cluster_library(c("dplyr", "purrr", "rlang", "rsofun"))
    
    ## distribute to to cores, making sure all data from a specific site is sent to the same core
    df_out <- df_drivers %>%
      dplyr::group_by( id = row_number() ) %>%
      tidyr::nest(input = c(sitename, params_siml, siteinfo, forcing, params_tile, params_species, params_soil, init_cohort, init_soil)) %>%
      multidplyr::partition(cl) %>% 
      dplyr::mutate(data = purrr::map( input, 
                                       ~run_lm3ppa_f_bysite(
                                         sitename       = .x$sitename[[1]], 
                                         params_siml    = .x$params_siml[[1]], 
                                         siteinfo       = .x$siteinfo[[1]], 
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
      dplyr::select( data )  %>% 
      tidyr::unnest( cols = c( data ))
    
  } else {
    
    df_out <- df_drivers %>% 
      dplyr::mutate(data = purrr::pmap(
        .,
        run_lm3ppa_f_bysite,
        makecheck = makecheck
      )) %>% 
      dplyr::select(sitename, data) 
    
  }
  
  # df_out <- df_out %>% as_tibble(.name_repair = 'unique')

  return(df_out)
}
