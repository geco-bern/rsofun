#' Collect all drivers
#'
#' Collect all drivers for site-level simulations into a nested data frame with one row for each site
#'
#' @param siteinfo A data frame containing site meta info (rows for sites). Required columns are: \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
#' @param params_siml A nested data frame with rows for each site containing simulation parameters by site.
#' @param meteo A nested data frame with rows for each site and meteo forcing data time series nested inside a column named \code{"data"}
#' @param fapar A nested data frame with rows for each site and fAPAR forcing data time series nested inside a column named \code{"data"}
#' @param co2 A nested data frame with rows for each site and CO2 forcing data time series nested inside a column named \code{"data"}
#' @param df_soiltexture
#'
#' @return
#' @export
#'
#' @examples mod <- collect_drivers_sofun( settings = settings, forcing, df_soiltexture )
#' 
collect_drivers_sofun <- function( siteinfo, params_siml, meteo, fapar, co2, df_soiltexture ){
  
  ## complement the setup settings
  siteinfo <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)
  
  ## create mega-df containing all forcing data and parameters that vary by site (not model parameters!)
  names_metainfo <- names(siteinfo)[-which(names(siteinfo) %in% c("sitename", "params_siml"))]
  df_mega <- siteinfo %>% 
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
    left_join(
      co2 %>% 
        rename(co2 = data),
      by = "sitename"
    ) %>% 
    mutate(df_soiltexture = purrr::map(as.list(seq(nrow(.))), ~return(df_soiltexture)))
  
  ## use only interpolated fapar and combine meteo data and fapar into a single nested column 'forcing'
  df_mega <- df_mega %>% 
    mutate(fapar = purrr::map(fapar, ~dplyr::select(., date, fapar))) %>% 
    mutate(co2   = purrr::map(co2  , ~dplyr::select(., date, co2))) %>% 
    mutate(forcing = purrr::map2(meteo, fapar, ~left_join( .x, .y, by = "date"))) %>% 
    mutate(forcing = purrr::map2(forcing, co2, ~left_join( .x, .y, by = "date"))) %>% 
    dplyr::select(-meteo, -fapar, -co2)
  
  ## interpolate to fill gaps in forcing time series
  myapprox <- function(vec){
    approx(vec, xout = 1:length(vec))$y
  }
  fill_na_forcing <- function(df){
    vars <- names(df)[-which(names(df)=="date")]
    df %>% 
      mutate_at(vars, myapprox)
  }
  df_mega <- df_mega %>% 
    mutate(forcing = purrr::map(forcing, ~fill_na_forcing(.)))

  return(df_mega)
}


  
  
