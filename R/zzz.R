# invisible functions to the general user
# used internally only load in start

is.nanull <- function(x) ifelse(is.na(x) | is.null(x), TRUE, FALSE)
