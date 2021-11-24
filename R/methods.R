
#' Plotting method for fit model output
#'
#' @param x input data generated using the run_*_f_bysite() function
#'  or generally calls which return the rsofun_fit class
#' @param ... additional parameters to pass
#' @return a scatterplot with fit statistics, plotting observed vs
#'  fitted GPP data
#'
#' @export
#' @import graphics

# plot data if requested
plot.rsofun_fit <- function(x, ...){
  stopifnot(inherits(x, "rsofun_fit"))
  stop("methods aren't functional yet")
}


#' Print summary values for fit model output
#'
#' @param object input data generated using the run_*_f_bysite() function
#'  or generally calls which return the rsofun_fit class
#' @param ... additional parameters to pass
#' @return a table with fit statistics, a data frame with summary statistics

#' @export

summary.rsofun_fit <- function(object, ...){
  stopifnot(inherits(object, "rsofun_fit"))
  stop("methods aren't functional yet")
}
