#' Get observational data for model evaluation
#'
#' Gets observational data for model evaluation, given the benchmarking variable, and data source used 
#' for the evaluation. This information is specified in the evaluation settings (argument \code{settings}).
#'
#' @param siteinfo A data frame containing site meta info (rows for sites). Required columns are: \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
#' @param settings A list specifying evaluation settings (see vignette eval_sofun.pdf for more information and examples)
#' @param adf A nested data frame with rows for sites and time series (annual) for each site nested inside the column \code{data}. 
#' @param mdf A nested data frame with rows for sites and time series (monthly) for each site nested inside the column \code{data}. 
#' @param ddf A nested data frame with rows for sites and time series (daily) for each site nested inside the column \code{data}. 
#' @param hrdf A nested data frame with rows for sites and time series (hourly) for each site nested inside the column \code{data}. 
#' @param hhdf A nested data frame with rows for sites and time series (half-hourly) for each site nested inside the column \code{data}. 
#'
#' @return A list containing nested data frames of observed values aggregated to several temporal scales (ddf for daily, xdf for X-daily 
#' (determined by argument \code{agg}), mdf for monthly, adf for annual).
#' @export
#'
#' @examples \dontrun{obs <- collect_obs_eval( settings = NULL, adf = NULL, mdf = NULL, ddf = NULL, hrdf = NULL, hhdf = NULL, agg = 8 )}
#' 
collect_obs_eval <- function( siteinfo, settings, adf = NULL, mdf = NULL, ddf = NULL, hrdf = NULL, hhdf = NULL ){

  if (is.null(adf)){
    rlang::abort("collect_obs_eval(): object provided by argument 'adf' is missing.")
  }
  
  if (is.null(ddf)){
    rlang::abort("collect_obs_eval(): object provided by argument 'ddf' is missing.")
  }
  
  if (is.null(mdf)){	
    rlang::abort("collect_obs_eval(): object provided by argument 'mdf' is missing.")
  }
  
  ##------------------------------------------------------------
  ## Aggregate daily data to multi-day periods
  ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
  ##------------------------------------------------------------
  ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
  calc_xdf <- function(ddf, agg){
    
    firstyear <- ddf %>% mutate(year = lubridate::year(date)) %>% pull(year) %>% min()
    lastyear  <- ddf %>% mutate(year = lubridate::year(date)) %>% pull(year) %>% max()
    listyears <- seq( ymd(paste0(firstyear, "-01-01")), ymd(paste0(lastyear, "-01-01")), by = "year" )    
    breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( agg, " days"), length.out = ceiling(365 / agg)) ) %>% Reduce(c,.)
    
    ## take mean across periods
    evalvars <- names(settings$benchmark)
    ddf %>% 
      mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
      tidyr::drop_na(inbin) %>% 
      group_by( inbin ) %>%
      summarise_at( vars(one_of(evalvars)), mean, na.rm=TRUE)

  }
  xdf <- ddf %>%
    mutate(data = purrr::map(data, ~calc_xdf(., agg = settings$agg)))
  
  ## complement info required for evaluation
  ddf <- siteinfo %>% 
    dplyr::select(sitename, lon, lat, elv, classid, c4, whc, koeppen_code, igbp_land_use, plant_functional_type) %>% 
    right_join(ddf, by = "sitename")
  
  xdf <- siteinfo %>% 
    dplyr::select(sitename, lon, lat, elv, classid, c4, whc, koeppen_code, igbp_land_use, plant_functional_type) %>% 
    right_join(xdf, by = "sitename")
  
  mdf <- siteinfo %>% 
    dplyr::select(sitename, lon, lat, elv, classid, c4, whc, koeppen_code, igbp_land_use, plant_functional_type) %>% 
    right_join(mdf, by = "sitename")
  
  adf <- siteinfo %>% 
    dplyr::select(sitename, lon, lat, elv, classid, c4, whc, koeppen_code, igbp_land_use, plant_functional_type) %>% 
    right_join(adf, by = "sitename")
  
  ## get breaks
  breaks <- xdf %>% tidyr::unnest(data) %>% pull(inbin) %>% as.character() %>% lubridate::ymd() %>% unique()
  
	return( list( ddf = ddf, xdf = xdf, mdf = mdf, adf = adf, breaks = breaks ) )

}
