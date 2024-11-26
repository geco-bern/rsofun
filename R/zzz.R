# invisible functions to the general user
# used internally only load in start

is.nanull <- function(x) ifelse(any(is.null(x), is.na(x)), TRUE, FALSE)

numeric_year <- function(x){
  y <- as.numeric(format(x, format="%Y"))
  doy <- as.numeric(format(x, format="%j")) - 1
  
  ifelse(y %% 4 == 0, 
         round(y + doy/366, 3), 
         round(y + doy/365, 3)
  )
}
