#' Run SOFUN and read output
#'
#' Runs the model and reads output in once.
#'
#' @param df_drivers A nested data frame with one row for each site and columns named according to the 
#' arguments of function \link{runread_sofun_f_bysite}
#' @param params_modl A named list of model parameters
#' @param makecheck A logical specifying whether checks are performed to verify forcings.
#' @param parallel A logical specifying whether simulations are to be parallelised (sending 
#' data from a certain number of sites to each core). Defaults to \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel computing. 
#' Defaults to 2.
#'
#' @return A data frame (tibble) with one row for each site and outputs stored in the nested
#' column \code{out_sofun}
#' @export
#'
#' @examples mod <- runread_sofun_f( df_drivers, params_modl, makecheck = TRUE, parallel = FALSE, ncores = 2 )
#' 
runread_sofun_f <- function( df_drivers, params_modl, makecheck = TRUE, parallel = FALSE, ncores = 2 ){

  if (parallel){

    cl <- multidplyr::new_cluster(2) %>% 
      multidplyr::cluster_assign(params_modl = params_modl) %>% 
      multidplyr::cluster_assign(makecheck = FALSE) %>% 
      multidplyr::cluster_library(c("dplyr", "purrr", "rlang", "rsofun"))
    
    ## distribute to to cores, making sure all data from a specific site is sent to the same core
    df_out <- df_drivers %>%
      dplyr::group_by( id = row_number() ) %>%
      tidyr::nest(input = c(sitename, params_siml, siteinfo, forcing, df_soiltexture)) %>%
      multidplyr::partition(cl) %>% 
      dplyr::mutate(out_sofun = purrr::map( input, 
                                     ~run_sofun_f_bysite(
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
      dplyr::select( out_sofun )  %>% 
      tidyr::unnest( cols = c( out_sofun ))
    
  } else {
  
    df_out <- df_drivers %>% 
      dplyr::mutate(out_sofun = purrr::pmap(
        .,
        run_sofun_f_bysite,
        params_modl = params_modl,
        makecheck = makecheck
      )) %>% 
      dplyr::select(sitename, out_sofun)
    
  }
  
  return(df_out)
}
