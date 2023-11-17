#' Initialises a tibble with dates
#'
#' Creates a tibble with rows for each date from \code{'yrstart'} to \code{'yrend'}
#' in \code{'yyyy-mm-dd'} format. Intervals of dates are specified by argument 
#'\code{'freq'}. 
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
#'  (in rows). Defaults to \code{"days"}. Any of \code{"days", "months", "years"}. If
#'  \code{freq = "months"} the 15\eqn{^{th}} day of the months is used as date,
#'  and if \code{freq = "years"} the 1\eqn{^{st}} of January of each year is returned.
#' @param endmoy An integer defining the end month-of-year of dates covered
#'  by the dataframe. Defaults to 12.
#' @param enddom An integer defining the end day-of-year of dates
#'  covered by the dataframe. Defaults to 31.
#' @param noleap Whether leap years are ignored, that is, whether the 29\eqn{^{th}} 
#' of February is removed. Defaults to \code{FALSE}.
#' 
#' @return A tibble with dates.
#' @export
#'
#' @examples
#'  ddf <- init_dates_dataframe( 2000, 2003, startmoy=1, startdoy=1,
#'   freq="days", endmoy=12, enddom=31, noleap=FALSE )

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
  date_range$year_dec <- numeric_year(date_range$date)

  # leap year filter
  if (noleap) {
    date_range <- dplyr::filter(date_range,
      !(format(date, "%m-%d") == "02-29")
      )
    
    }

  return(date_range)
}
