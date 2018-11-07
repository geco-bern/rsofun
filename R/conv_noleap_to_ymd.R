## Copied from https://github.com/cran/wux/blob/master/R/ReadNetCdfTimeData.R

conv_noleap_to_ymd <- function( time, since, calender.days=365 ){
  ## Converts timesteps from NetCDF file having 365 or 360 days calendar to
  ## POSIXt class timevector. The reason for this function is, that cannot simply
  ## add the timevector to the POSIX date we wish, as this would lead to complete
  ## nonsense data! Therefore we add the years and days seperately and leave out
  ##  may 31st, july 31st aug 31st, oct 31st and dec 31st (360 days) and
  ## march 31st if leapyear.
  ## For 365 days calendar we leave out feb 29th if being leap year.
  ##
  ## Args:
  ##   time: A vector with timesteps, which once was the time variable of a
  ##         NetCDF file. These timesteps have to be on DAILY basis!
  ##   since: POSIXt object, time starts with.
  ##   calender.days: Character or numeric. Calendar type. Can be "365" or "360"
  ##
  ## Returns:
  ##   POSIXct vector of dates within NetCDF file, having same length as NetCDF
  ##   time dimension.
  ##
  ## History:
  ##   2010-11-29 | Original code (thm)
  ##   2011-12-01 | Deleted hack, which was just not correct (it shifted one day back
  ##                ward, which for monthly data didnt matter, so data are processed
  ##                the same way) (thm)
  ##   2011-12-16 | handle properly the cases of 360 days and 365 days (thm)
  ##   2014-04-09 | changed to modulo calculation due to rounding error (thm)

  IsLeapYear <- function(year){
    ## small helperfunction returning boolean vector leapyear true/false
    ## http://en.wikipedia.org/wiki/Leap_year
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
  }



  ## convert arguments to useful format
  calender.days <- as.numeric(calender.days)
  since <- as.POSIXlt(since)

  ## get vector containing the "years since" and "day in that year"
  n.of.years <- time/calender.days
  year <- floor(n.of.years)

  ## merge "years since" and "day in that year" to a POSIXct date vector
  date.posix <- since
  ## get years since ... (by adding years)
  date.posix$year <- since$year + year
  ## boolean vector indicating if year is leapyear
  is.leapyear <- IsLeapYear(as.integer(format(date.posix, format = "%Y")))

  ## getting days within this year
  days <- time %% calender.days  ## OLD calculation: (n.of.years - year) * calender.days
  ## now comes the crucial part:
  ## we shift the days for 360days calendar and 365days calendar so, that:
  ## case 360days: all 31st (except jan and mar) disappear (for leapyears all
  ##               except jan)
  ## case 365days: for leapyears feb 29th disappears
  if (calender.days == 360){
    days[days >= 150 & !is.leapyear] <- days[days >= 150 & !is.leapyear] + 1#may 31th
    days[days >= 211 & !is.leapyear] <- days[days >= 211 & !is.leapyear] + 1#jul 31th
    days[days >= 242 & !is.leapyear] <- days[days >= 242 & !is.leapyear] + 1#aug 31th
    days[days >= 303 & !is.leapyear] <- days[days >= 303 & !is.leapyear] + 1#oct 31th
    days[days >= 364 & !is.leapyear] <- days[days >= 364 & !is.leapyear] + 1#dec 31th

    days[days >= 90  & is.leapyear] <- days[days >= 90  & is.leapyear] + 1 #mar 31th
    days[days >= 151 & is.leapyear] <- days[days >= 151 & is.leapyear] + 1 #may 31th
    days[days >= 212 & is.leapyear] <- days[days >= 212 & is.leapyear] + 1 #jul 31th
    days[days >= 243 & is.leapyear] <- days[days >= 243 & is.leapyear] + 1 #aug 31th
    days[days >= 304 & is.leapyear] <- days[days >= 304 & is.leapyear] + 1 #oct 31th
    days[days >= 365 & is.leapyear] <- days[days >= 365 & is.leapyear] + 1 #dec 31th
  }
  else if (calender.days == 365){
    days[days >= 60  & is.leapyear] <- days[days >= 60  & is.leapyear] + 1 #feb 29th
  }
  else {
    stop("INVALID CALENDAR TYPE IN conv_noleap_to_ymd")
  }

  ## add days to POSIX object
  date.posix <- date.posix + days * 24 * 3600

  ## short output summary
  n.years.approx <- as.integer(strftime( tail(date.posix, 1), "%Y")) -
    as.integer(strftime( date.posix[1], "%Y")) + 1
  ## cat("Range in NetCDF file: ", as.character(date.posix[1]), " to ",
  ##     as.character(tail(date.posix, 1)),  " (~",
  ##     n.years.approx, " years)\n", sep="")

  return( ymd(date.posix) )
}
