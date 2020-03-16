#' Complements the setup settings
#'
#' Complements the settings based on the site metainfo CSV file or data frame.
#'
#' @param siteinfo A character string specifying the path to the site meta info file, or a dataframe containing the site meta info.
#' @param params_siml A named list containing the simulation parameters for SOFUN.
#'
#' @return A dataframe (tibble) containing the site meta info, complemented by column 'params_siml' which is a nested list of complemented simulation parameters.
#' @export
#'
#' @examples setup <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)
#' 
prepare_setup_sofun <- function( siteinfo, params_siml ){

  ##-----------------------------------------------------------
  ## Ensemble: multiple site-scale simulations that "go toghether"
  ## In this case, <settings$name> is the name of the ensemble (e.g., "fluxnet2015")
  ##-----------------------------------------------------------
  ## Read info that varies between sites from the meta information file <siteinfo>:
  ## - site name, must be: column number 1 
  ## - longitude of site, column must be named 'lon'
  ## - latitude of site, column must be named 'lat'
  ## - elevation of site, column must be named 'elv'
  ## - years for which simulation is to be done (corresponding to data availability from site), 
  ##   requires two columns named 'year_start' and 'year_end'.
  if (is.character(siteinfo)){
    if (!file.exists(siteinfo)) rlang::abort( "prepare_setup_sofun(): Path specified by siteinfo does not exist." )
    siteinfo <- readr::read_csv( siteinfo )
  }

  ##--------------------------------------
  ## Complement settings with meta info for each site
  ##--------------------------------------
  siteinfo <- siteinfo %>% 

    ## clean up and complement
    mutate(year_start = as.numeric(year_start), year_end = as.numeric(year_end)) %>% 
    mutate(date_start = lubridate::ymd( paste0( as.character( siteinfo$year_start ), "-01-01" ) ),
           date_end   = lubridate::ymd( paste0( as.character( siteinfo$year_end ), "-12-31" ) ))

  ## add simulation parameters as a list nested in 'siteinfo'
  siteinfo <- params_siml %>%
    dplyr::bind_cols(.) %>% 
    dplyr::mutate(tmp = 1) %>% 
    dplyr::right_join(mutate(siteinfo, tmp = 1), by = "tmp") %>% 
    dplyr::select(names(siteinfo), names(params_siml)) %>% 
    dplyr::mutate(nyeartrend = year_end - year_start + 1) %>%
    dplyr::rename(firstyeartrend = year_start) %>% 
    tidyr::nest(params_siml = c(names(params_siml), "firstyeartrend", "nyeartrend"))
  
  return(siteinfo)

}
