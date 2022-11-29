#' Initialises a tibble with dates
#'
#' Creates a tibble with rows for each date (\code{\link[lubridate:ymd]{lubridate::ymd}} object from 
#' library \code{lubridate}) from \code{'yrstart'} to \code{'yrend'}. Intervals of dates is 
#' specified by argument \code{'freq'}. 
#'
#' @param yrstart An integer defining the start year
#'  of dates covered by the dataframe.
#' @param yrend An integer defining the end year of dates
#'  covered by the dataframe.
#' @param startmoy An integer defining the start month-of-year of dates
#'  covered by the dataframe. Defaults to 1.
#' @param startdoy An integer defining the start day-of-year of
#'  dates covered by the dataframe. Defaults to 1.
#' @param freq A character string specifying the time steps of dates
#'  (in rows). Defaults to \code{"days"}. Any of \code{"days", "months"}.
#' @param endmoy An integer defining the end month-of-year of dates covered
#'  by the dataframe. Defaults to 12.
#' @param enddom An integer defining the end day-of-year of dates
#'  covered by the dataframe. Defaults to 31.
#' @param noleap Whether leap years are ignored. Defaults to \code{FALSE}.
#'
#' @import lubridate
#' 
#' @return A tibble with dates.
#' @export
#'
#' @examples
#' \dontrun{
#'  ddf <- init_dates_dataframe( 2000, 2003, startmoy=1, startdoy=1,
#'   freq="days", endmoy=12, enddom=31, noleap=FALSE )
#' }

init_dates_dataframe <- function(
  yrstart,
  yrend,
  startmoy=1,
  startdoy=1,
  freq="days",
  endmoy=12,
  enddom=31,
  noleap=FALSE ){
  
  if (freq=="days"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-01",
              yrstart, startmoy)) + (startdoy - 1)
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-%02d",
              yrend, endmoy, enddom))

  } else if (freq=="months"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-15",
              yrstart, startmoy))
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-15",
              yrend, endmoy))
  
  } else if (freq=="years"){
    
    start_date <- as.Date(
      sprintf("%04d-%02d-01",
              yrstart, 1))
    
    end_date   <- as.Date(
      sprintf("%04d-%02d-01",
              yrend, 7))    
  }

  # define date range
  date_range <- data.frame(
    date = seq.Date(
      from = start_date,
      to = end_date,
      by = freq
    ))
  
  # convert to decimal date
  date_range$year_dec <- lubridate::decimal_date(date_range$date)

  # leap year filter
  if (noleap) {
    date_range <- dplyr::filter(date_range,
      !(lubridate::month(date) == 2 & lubridate::mday(date) == 29)
      )
    
    }

  return(date_range)
}
