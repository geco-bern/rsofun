##--------------------------------------------------------------------
## Creates a dataframe (tibble) with rows for each date (ymd object from 
## library lubridate) from 'yrstart' to 'yrend'. Intervals of dates is 
## specified by argument 'freq'. 
##--------------------------------------------------------------------
init_dates_dataframe <- function( yrstart, yrend, startmoy=1, startdoy=1, freq="days", endmoy=12, enddom=31, noleap=FALSE ){

  if (freq=="days"){
    startdate <- ymd( paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-01" ) ) + days( startdoy - 1 )
    enddate   <- ymd( paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-", sprintf( "%02d", enddom  ) ) )    
  } else if (freq=="months"){
    ## date is always the 15th of each month
    startdate <- ymd( paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-15" ) )
    enddate   <- ymd( paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-15" ) )    
  }

  ddf <-  tibble( date=seq( from = startdate, to = enddate, by = freq ) ) %>% 
          mutate( ndayyear = ifelse( leap_year(year(date)), 366, 365  ) ) %>%
          mutate( year_dec = year(date) + (yday(date) - 1) / ndayyear ) %>% 
          dplyr::select( -ndayyear )

  if (noleap) ddf <- ddf %>% dplyr::filter( !( month(date)==2 & mday(date)==29 ) )

  return( ddf )

}
