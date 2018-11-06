calib_pmodel <- function( df, temp_cutoff, soilm_cutoff, kphio_used ){

	## Returns the optimised quantum yield efficiency parameter (kphio_opt),
	## given modelled GPP (using arbitrary quantum yield eff. par.) and observed GPP, 
	## both given in dataframe 'df' and named 'gpp' and 'GPP_NT_VUT_REF', resp.

	require(ncdf4)

	source("analyse_modobs.R")

	## Get initial number of data points
	ndays_0 <- nrow( df )

	## Remove cold days
	df <- df %>% filter( temp > temp_cutoff )
	ndays_1 <- nrow( df )
	# print( paste0( "total number of days after removing cold days: ", ndays_1, " (", format( ndays_1/ndays_0*100, digits=2 ), "% remaining)") )

	## Remove dry days
	df <- df %>% filter( soilm_mean > soilm_cutoff )
	ndays_2 <- nrow( df )
	# print( paste0( "total number of days after removing dry days: ", ndays_2, " (", format( ndays_2/ndays_0*100, digits=2 ), "% remaining)") )

	## Plot observed vs. modelled
	stats <- with( df, analyse_modobs( GPP_NT_VUT_REF, gpp, yintersect0=TRUE, xlab="modelled", ylab="observed", do.plot=FALSE ) )

	## Optimal correction factor for quantum yield efficiency parameter:
	kphio_corr <- stats$linmod$coefficients

	## Calculate optimal apparent quantum yield efficiency parameter:
	kphio_opt <- kphio_used * kphio_corr

	## Construct output list
	out <- list( kphio_opt=kphio_opt, ndays=ndays_2, kphio_corr=kphio_corr )

	return( out )
}
