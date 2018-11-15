##------------------------------------------------------------
## Plot mean per site -> spatial correlation
##------------------------------------------------------------
modobs_spatial <- plot_modobs_spatial <- function( meandf, makepdf=TRUE ){
	par(las=1, mar=c(4,4.5,4,1))
	dir <- "fig/"
	if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
	if (makepdf) {filn <- paste0(dir, "modobs_spatial.pdf")} else  {filn <- NA}
	modobs_spatial <- with( meandf, analyse_modobs( 
																				gpp_mod, 
																				gpp_obs, 
																				heat = FALSE, 
																				col = "black", 
																				ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
																				xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
																				plot.fil = filn,
																				plot.title = "Spatial correlation"
																				) )
	# abline( linmod_meandf, col="red")
	return(modobs_spatial)
}

##------------------------------------------------------------
## Combined spatial - IAV correlation
##------------------------------------------------------------
modobs_spatial <- plot_modobs_spatial_annual <- function( meandf, linmod_meandf, adf_stats, makepdf=FALSE ){
  if (makepdf) pdf("fig/modobs_spatial_annual.pdf")
    par(las=1, mar=c(4,4.5,4,1))
    with( meandf, plot( gpp_mod, gpp_obs, xlim = c(0,4000), ylim = c(0,4000), pch=16, col=rgb(0,0,0,0.5), type = "n", ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ) ) )
		abline( linmod_meandf, col="red")
		out <- adf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = . ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		title( "Spatial/annual correlation" )
	if (makepdf) dev.off()

	# ## Histogram of slopes
	# ##------------------------------------------------------------
	# ## (Uncomment to plot as inset in spatial-IAV plot) 
	# # u <- par("usr")
	# # v <- c(
	# #   grconvertX(u[1:2], "user", "ndc"),
	# #   grconvertY(u[3:4], "user", "ndc")
	# # )
	# # v_orig <- v
	# # v <- c( v[1]+0.03, v[1]+0.2*v[2], v[3]+0.50*v[4], v[3]+0.72*v[4] )
	# # par( fig=v, new=TRUE, mar=c(0,0,0,0), mgp=c(3,0.5,0) )
	# if (makepdf) pdf("fig/hist_slopes_anomalies_annual.pdf")
	# 	hist( adf_stats$slope, xlim=c(-5,5), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 50, xlab="slope" )
	# 	abline( v=1.0, col="red" )
	# 	axis( 1, cex.axis=1.0, xlab="slope" )
	# 	title( "Slopes of annual regressions" )
	# if (makepdf) dev.off()

	# ## Histogram of R2
	# ##------------------------------------------------------------
	# ## (Uncomment to plot as inset in spatial-IAV plot) 
	# # u <- par("usr")
	# # v <- c(
	# #   grconvertX(u[1:2], "user", "ndc"),
	# #   grconvertY(u[3:4], "user", "ndc")
	# # )
	# # v_orig <- v
	# # v <- c( v[1]+0.03, v[1]+0.2*v[2], v[3]+0.50*v[4], v[3]+0.72*v[4] )
	# # par( fig=v, new=TRUE, mar=c(0,0,0,0), mgp=c(3,0.5,0) )
	# if (makepdf) pdf("fig/hist_r2_anomalies_annual.pdf")
	# 	hist( adf_stats$rsq, xlim=c(-1,1), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 12, xlab= bquote( italic(R)^2 ) )
	# 	abline( v=1.0, col="red" )
	# 	axis( 1, cex.axis=1.0, xlab = bquote( italic(R)^2 ) )
	# 	title( bquote( bold(Slopes ~ of ~ italic(R)^2) ) )
	# if (makepdf) dev.off()
	
}


##------------------------------------------------------------
## IAV correlation: x_(y,i) - mean_y( x_(y,i) )
##------------------------------------------------------------
plot_modobs_anomalies_annual <- function( iavdf, iavdf_stats, makepdf=FALSE ){
  if(makepdf) pdf( "fig/modobs_anomalies_annual.pdf" )
		par(las=1)
		modobs_anomalies_annual <- with( iavdf, analyse_modobs(gpp_mod, 
			gpp_obs, 
			heat = FALSE,
			ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
			plot.title = "IAV correlation"
		 ))
		out <- iavdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(0,0,1,0.3) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
	if(makepdf) dev.off()
	return(modobs_anomalies_annual)
}


##------------------------------------------------------------
## IDV (interday variability) correlation: x_(d,i) - mean_d( x_(d,i) )
##------------------------------------------------------------
plot_modobs_anomalies_daily <- function( idvdf, idvdf_stats, makepdf=FALSE){
	if (makepdf) pdf("fig/modobs_anomalies_daily.pdf")
		modobs_anomalies_daily <- with( idvdf, analyse_modobs( 
		  gpp_mod, 
		  gpp_obs, 
		  col=rgb(0,0,0,0.05), 
		  ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
		  xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )  
		  ))
		out <- idvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(0,0,1,0.05) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		title( "IDV correlation" )
	if (makepdf) dev.off()
	
	## histogram of daily anomalies from mean seasonal cycle based on DOY
	##------------------------------------------------------------
	if (makepdf) pdf("fig/hist_anomalies_daily.pdf")
		par(las=1)
		with( idvdf, hist( gpp_obs, breaks = 20, col = rgb(0,0,0,0.3), freq = FALSE, main = "Daily anomalies", xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ) ) )
		with( idvdf, hist( gpp_mod, breaks = 20, col = rgb(1,0,0,0.3), freq = FALSE, add = TRUE ) )
    mtext( bquote( sigma[obs] == .(format( sd(idvdf$gpp_obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 )	
    mtext( bquote( sigma[mod] == .(format( sd(idvdf$gpp_mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )	
    legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
  if (makepdf) dev.off()

  return(modobs_anomalies_daily)

}		


##------------------------------------------------------------
## IXV correlation: x_(x,i) - mean_x( x_(x,i) )
##------------------------------------------------------------
plot_modobs_anomalies_xdaily <- function( ixvdf, ixvdf_stats, makepdf=FALSE ){
  if (makepdf) pdf("fig/modobs_anomalies_xdaily.pdf")
		modobs_anomalies_xdaily <- with( ixvdf, analyse_modobs(
		  gpp_mod, 
		  gpp_obs, 
		  col=rgb(0,0,0,0.05), 
		  ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
		  xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )
		  ))
		out <- ixvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(0,0,1,0.1) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		title( "IXV correlation" )
	if (makepdf) dev.off()

	## histogram of X-daily anomalies from mean seasonal cycle based on XOY
	##------------------------------------------------------------
	if (makepdf) pdf("fig/hist_anomalies_xdaily.pdf")
		par(las=1)
		with( ixvdf, hist( gpp_obs, breaks = 20, col = rgb(0,0,0,0.3), freq = FALSE, main = "Anomalies in X-day periods", xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ), ylim = c(0,0.45) ) )
		with( ixvdf, hist( gpp_mod, breaks = 20, col = rgb(1,0,0,0.3), freq = FALSE, add = TRUE ) )
    mtext( bquote( sigma[obs] == .(format( sd(ixvdf$gpp_obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 )	
    mtext( bquote( sigma[mod] == .(format( sd(ixvdf$gpp_mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )	
    legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
  if (makepdf) dev.off()
  return(modobs_anomalies_xdaily)
}  


##------------------------------------------------------------
## Mean seasonal cycle by day of year (DOY)
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_meandoy <- function( meandoydf, meandoydf_stats, makepdf=FALSE ){
	if (makepdf) pdf( "fig/modobs_meandoy.pdf" )
	modobs_meandoy <- with( meandoydf, 
		analyse_modobs( 
			mod_mean, 
			obs_mean, 
			heat=TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Mean-by-DOY correlation"
			) )
	if (makepdf) dev.off()
	return(modobs_meandoy)
}


## mean seasonal cycle by site (selected sites only)
plot_by_doy_allsites <- function( meandoydf_stats, makepdf=FALSE ){
	system( "mkdir -p fig/meandoy_bysite" )
	# mylist <- readr::read_csv("myselect_fluxnet2015.csv") %>% filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
	mylist <- c("AU-Tum", "CA-NS3", "CA-NS6", "CA-Obs", "DE-Geb", "DE-Hai", "DE-Kli", "FI-Hyy", "FR-Fon", "FR-LBr", "FR-Pue", "IT-Cpz", "NL-Loo", "US-Ha1", "US-MMS", "US-UMB", "US-WCr")
	tmp <- purrr::map( filter( meandoydf_stats, sitename %in% mylist )$data, ~plot_by_doy_bysite(., makepdf = makepdf) )
}


## mean seasonal cycle by climate zone and hemisphere
plot_by_doy_allzones <- function( meandoydf_byclim_stats, makepdf=FALSE ){
	system( "mkdir -p fig/meandoy_byzone" )
	tmp <- purrr::map( meandoydf_byclim_stats$data, ~plot_by_doy_byzone(., makepdf = makepdf ) )
}


##------------------------------------------------------------
## Mean seasonal cycle by x-day-period of year (XOY)
##------------------------------------------------------------
## observed vs. modelled
modobs_meanxoy <- plot_modobs_meanxoy <- function( meanxoydf, makepdf=FALSE ){
	if (makepdf) pdf("fig/modobs_meanxoy.pdf")
	modobs_meanxoy <- with( meanxoydf, 
		analyse_modobs( 
			mod_mean, 
			obs_mean, 
			heat=TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Mean-by-XOY correlation"
		) )		
	if (makepdf) dev.off()
	return(modobs_meanxoy)					   
}		


plot_by_xoy_allsites <- function( meanxoydf_stats, makepdf=FALSE ){
	system( "mkdir -p fig/meanxoy_bysite" )
	mylist <- readr::read_csv("myselect_fluxnet2015.csv") %>% filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
	tmp <- purrr::map( filter( meanxoydf_stats, sitename %in% mylist )$data, ~plot_by_xoy_bysite(., makepdf = TRUE ) )
}


##------------------------------------------------------------
## Daily values (absolute)
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_daily <- function( ddf, makepdf=FALSE, ... ){
	if (makepdf) pdf("fig/modobs_daily.pdf")
	modobs_ddf <- with( ddf, 
		analyse_modobs( 
			gpp_mod, 
			gpp_obs, 
			heat=TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Correlation of daily GPP",
			...
		) )
	if (makepdf) dev.off()
	return( modobs_ddf )
}

##------------------------------------------------------------
## Monthly values (absolute)
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_monthly <- function( mdf, makepdf=FALSE, ... ){
	if (makepdf) pdf("fig/modobs_monthly.pdf")
	modobs_mdf <- with( mdf, 
		analyse_modobs( 
			gpp_mod, 
			gpp_obs, 
			heat = TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Correlation of monthly GPP",
			...
		) )
	if (makepdf) dev.off()
	return( modobs_mdf )
}


##------------------------------------------------------------
## Annual values (absolute)
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_annual <- function( adf, makepdf=FALSE, ... ){
	if (makepdf) pdf("fig/modobs_annual.pdf")
	modobs_adf <- with( adf, 
		analyse_modobs( 
			gpp_mod, 
			gpp_obs, 
			heat = FALSE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
			plot.title = "Correlation of annual GPP",
			...
		) )
	if (makepdf) dev.off()
	return( modobs_adf )
}


##------------------------------------------------------------
## Aggregated values (absolute) to X-day periods
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_xdaily <- function( xdf, makepdf=FALSE, ... ){
	if (makepdf) pdf("fig/modobs_xdaily.pdf")
	modobs_xdf <- with( xdf, 
		analyse_modobs( 
			gpp_mod, 
			gpp_obs, 
			heat=TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Correlation of mean GPP in X-day periods",
			...
			) )
	if (makepdf) dev.off()
	return(modobs_xdf)
}


plot_by_doy_bysite <- function( df, makepdf=FALSE ){
	if (makepdf) pdf( paste0( "fig/meandoy_bysite/meandoy_bysite_", df$site[1], ".pdf" ))
	  par(las=1)
	  yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
	  plot(  df$doy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
	  polygon( c(df$doy, rev(df$doy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
	  lines( df$doy, df$mod_mean, col="red", lwd=1.75 )
	  polygon( c(df$doy, rev(df$doy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
	  title( df$site[1] )
  if (makepdf) dev.off()
}


plot_by_doy_byzone <- function( df, makepdf=FALSE ){
	if (makepdf) filn <- paste0( "fig/meandoy_byzone/meandoy_byzone_", df$climatezone[1], ".pdf" )
	if (makepdf) print( paste( "Creating plot", filn ) )
	if (makepdf) pdf( filn )
	  par(las=1)
	  yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
	  plot(  df$doy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
	  polygon( c(df$doy, rev(df$doy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
	  lines( df$doy, df$mod_mean, col="red", lwd=1.75 )
	  polygon( c(df$doy, rev(df$doy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
	  title( df$climatezone[1] )
	  mtext( bquote( italic(N) == .( df$nsites[1])), side=3, line=1, cex=1.0, adj=1.0 )
  if (makepdf) dev.off()
}


plot_by_xoy_bysite <- function( df, makepdf=FALSE ){
	if (makepdf) pdf( paste0( "fig/meanxoy_bysite/meanxoy_bysite_", df$site[1], ".pdf" ))
	  par(las=1)
	  yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
	  plot(  df$xoy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
	  polygon( c(df$xoy, rev(df$xoy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
	  lines( df$xoy, df$mod_mean, col="red", lwd=1.75 )
	  polygon( c(df$xoy, rev(df$xoy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
	  title( df$site[1] )
  if (makepdf) dev.off()
}
