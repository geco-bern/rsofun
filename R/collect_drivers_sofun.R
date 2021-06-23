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
  
  ## check if all required variables are available
  if (!("snow" %in% names(meteo$data[[1]]))){
    rlang::warn("Variable 'snow' missing in meteo data frame. Assuming zero for all dates. \n")
    meteo <- meteo %>% mutate(data = purrr::map(data, ~mutate(., snow = 0)))
  }
  if (!("rain" %in% names(meteo$data[[1]]))){
    rlang::warn("Variable 'rain' missing in meteo data frame. Assuming equal to 'prec' for all dates. \n")
    meteo <- meteo %>% mutate(data = purrr::map(data, ~mutate(., rain = prec)))
  }
  
  vars_req <- c("ppfd", "rain", "snow", "prec", "temp", "patm", "vpd", "ccov")
  vars_missing <- vars_req[!(vars_req %in% names(meteo %>% unnest(data)))]
  if (length(vars_missing)) rlang::abort(paste("Aborting. Variables missing in meteo data frame:", paste(vars_missing, collapse = ", ")))
  
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
  
  ## drop sites for which forcing data is missing for all dates
  count_notna <- function(df){
    df %>% 
      ungroup() %>% 
      summarise(across(c("ppfd", "rain", "snow", "prec", "temp", "patm", "vpd", "ccov", "fapar", "co2"), ~sum(!is.na(.)))) %>% 
      pivot_longer(cols = 1:10, names_to = "var", values_to = "n_not_missing")
  }
  
  df_missing <- df_mega %>% 
    mutate(df_count = purrr::map(forcing, ~count_notna(.))) %>% 
    dplyr::select(sitename, df_count) %>% 
    unnest(df_count) %>% 
    dplyr::filter(n_not_missing < 365)
  
  if (nrow(df_missing)>0){
    rlang::warn("Missing values found in forcing data frame:")
    print(df_missing)
    rlang::warn("Respective sites are dropped from all drivers data frame.")
    df_mega <- df_mega %>% 
      dplyr::filter(!(sitename %in% pull(df_missing, sitename)))
  }
    
  ## interpolate to fill gaps in forcing time series
  myapprox <- function(vec){
    approx(vec, xout = 1:length(vec))$y
  }
  
  fill_na_forcing <- function(df){
    
    vars <- names(df)[-which(names(df)=="date")]
    df <- df %>% 
      mutate_at(vars, myapprox)
    
    ## fill remaining gaps with mean seasonal cycle
    add_doy <- function(string){paste0(string, "_doy")}
    df_meandoy <- df %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      group_by(doy) %>% 
      summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
      rename_with(.fn = add_doy, .cols = one_of("ppfd", "rain", "snow", "prec", "temp", "patm", "vpd", "ccov", "fapar", "co2")) %>% 
      dplyr::select(doy, one_of("ppfd_doy", "rain_doy", "snow_doy", "prec_doy", "temp_doy", "patm_doy", "vpd_doy", "ccov_doy", "fapar_doy", "co2_doy"))
    df <- df %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      left_join(df_meandoy, by = "doy") %>% 
      mutate(ppfd = ifelse(is.na(ppfd), ppfd_doy, ppfd),
             rain = ifelse(is.na(rain), rain_doy, rain),
             snow = ifelse(is.na(snow), snow_doy, snow),
             prec = ifelse(is.na(prec), prec_doy, prec),
             temp = ifelse(is.na(temp), temp_doy, temp),
             patm = ifelse(is.na(patm), patm_doy, patm),
             vpd = ifelse(is.na(vpd), vpd_doy, vpd),
             ccov = ifelse(is.na(ccov), ccov_doy, ccov),
             fapar = ifelse(is.na(fapar), fapar_doy, fapar),
             co2 = ifelse(is.na(co2), co2_doy, co2)) %>% 
      dplyr::select(-ends_with("_doy"))
    
    return(df)
  }
  
  df_mega <- df_mega %>% 
    mutate(forcing = purrr::map(forcing, ~fill_na_forcing(.))) %>% 
    dplyr::select(sitename, forcing, params_siml, siteinfo, df_soiltexture)

  return(df_mega)
}


  
  
