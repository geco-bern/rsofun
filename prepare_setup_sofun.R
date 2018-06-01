prepare_setup_sofun <- function( settings ){

  require(readr)
  require(dplyr)
  require(lubridate)

  ## Get additional information from meta information file
  siteinfo <- read_csv( settings$path_siteinfo )

  ## Start year
  year_start <- sapply( siteinfo[,1], function(x) siteinfo$year_start[ which(siteinfo[,1]==x) ] ) %>%
    as.list()
  date_start <- lapply( year_start, function(x) ymd( paste0( x, "-01-01" )) )
  sitenam_colnam <- names(siteinfo)[1]
  names(date_start) <- siteinfo[,1][[sitenam_colnam]]

  ## End year
  year_end <- sapply( siteinfo[,1], function(x) siteinfo$year_end[ which(siteinfo[,1]==x) ] ) %>%
    as.list()
  date_end <- lapply( year_end, function(x) ymd( paste0( x, "-01-01" )) )
  sitenam_colnam <- names(siteinfo)[1]
  names(date_end) <- siteinfo[,1][[sitenam_colnam]]
  
  ## Complement settings list
  settings$sitenames  <- siteinfo[,1][[sitenam_colnam]]
  settings$date_start <- date_start
  settings$date_end   <- date_end

  return(settings)

}
