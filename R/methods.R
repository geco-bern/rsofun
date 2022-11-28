
#' Plotting method for rsofun_fit model output
#'
#' @param x Input data generated using the \code{\link{run_pmodel_f_bysite}}
#' or \code{\link{run_biomee_f_bysite}} function,
#' or generally calls which return the \code{rsofun_fit} class.
#' @param ... Additional parameters to pass.
#' @return A scatterplot with fit statistics, plotting observed vs
#'  fitted GPP data.
#'
#' @export
#' @import graphics

# plot data if requested
plot.rsofun_fit <- function(x, ...){
  stopifnot(inherits(x, "rsofun_fit"))
  stop("methods aren't functional yet")
}


#' Print summary values for rsofun_fit model output
#'
#' @param object Input data generated using the \code{\link{run_pmodel_f_bysite}}
#' or \code{\link{run_biomee_f_bysite}} function,
#'  or generally calls which return the \code{rsofun_fit} class.
#' @param ... Additional parameters to pass.
#' @return A table with fit statistics, a data frame with summary statistics.

#' @export

summary.rsofun_fit <- function(object, ...){
  stopifnot(inherits(object, "rsofun_fit"))
  stop("methods aren't functional yet")
}
