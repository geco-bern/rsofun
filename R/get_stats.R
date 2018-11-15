##-------------------------------------------------------------------------
#' Get statistics
#'
#' Returns multiple model performance statistics
#'
#' @param mod A numeric vector of modelled values 
#' @param obs A numeric vector of observed values
#'
#' @return A single row of a data frame (tibble) with columns corresponding to statistics
#' @export
#'
#' @examples stats <- get_stats( mod, obs )
#' 
get_stats <- function( mod, obs ){

	linmod <- lm( obs ~ mod )
	linmod_sum <- summary( linmod )
	rsq <- linmod_sum$adj.r.squared
	rmse <- sqrt( mean( (mod - obs)^2, na.rm=TRUE ) )
	slope <- coef(linmod)[2]
	nvals <- sum( !is.na(mod) & !is.na(obs) )
	bias <- mean( (mod - obs), na.rm=TRUE )
	return( tibble( rsq=rsq, rmse=rmse, slope=slope, bias=bias, nvals=nvals ) )

}
