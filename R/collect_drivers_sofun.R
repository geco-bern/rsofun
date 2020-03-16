#' Collect all drivers
#'
#' Collect all drivers for site-level simulations into a nested data frame with one row for each site
#'
#' @param settings A named list containing the simulation settings (see vignette_rsofun.pdf for more information and examples).
#' @param meteo A data nested data frame with rows for each site and meteo forcing data time series nested inside a column named \code{"data"}
#' @param fapar A data nested data frame with rows for each site and fAPAR forcing data time series nested inside a column named \code{"data"}
#' @param df_soiltexture
#'
#' @return
#' @export
#'
#' @examples mod <- collect_drivers_sofun( settings = settings, forcing, df_soiltexture )
#' 
collect_drivers_sofun <- function( settings, meteo, fapar, df_soiltexture ){
  
  ## create mega-df containing all forcing data and parameters that vary by site (not model parameters!)
  names_metainfo <- names(settings)[-which(names(settings) %in% c("sitename", "params_siml"))]
  df_mega <- settings %>% 
    tidyr::nest(siteinfo = names_metainfo) %>% 
    left_join(
      meteo %>% 
        rename(meteo = data),
      by = "sitename"
    ) %>% 
    left_join(
      fapar %>% 
        rename(fapar = data),
      by = "sitename"
    ) %>% 
    mutate(df_soiltexture = purrr::map(as.list(seq(nrow(.))), ~return(df_soiltexture)))
  
  return(df_mega)
}
  
  
