# invisible functions to the general user
# used internally only load in start

is.nanull <- function(x) ifelse(any(is.null(x), is.na(x)), TRUE, FALSE)
