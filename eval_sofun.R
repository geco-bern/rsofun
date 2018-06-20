# load( "mod.Rdata" )
# successcodes <- read_csv( "successcodes.csv" )

eval_sofun <- function( mod, settings_eval, settings_sims ){
	
  require(dplyr)
  require(purrr)
  require(lubridate)
  require(tidyr)

  source("remove_outliers.R")
  source("analyse_modobs.R")
  
	if (settings_eval$benchmark$gpp=="fluxnet2015"){
		##-------------------------------------------------------
		## GPP EVALUATION AGAINST FLUXNET 2015 DATA
		## Evaluate model vs. observations for decomposed time series
		## into:
		## - spatial
		## - inter-annual
		## - multi-year trend
		## - seasonal (different time scales: daily/weekly/monthly)
		## - anomalies (different time scales: daily/weekly/monthly)
		##-------------------------------------------------------
	  source("get_obs_bysite_gpp_fluxnet2015.R")

	  ## get sites for which no model output is available
	  missing_mod <- purrr::map_lgl( mod, ~identical(., NA ) ) %>% which() %>% names()
	  settings_eval$sitenames_used <- settings_eval$sitenames[ -which( settings_eval$sitenames %in% missing_mod ) ]
	  
	  ##------------------------------------------------------------
	  ## Read annual observational data from files (from annual files!).
	  ##------------------------------------------------------------
	  ## loop over sites to get data frame with all variables
	  print("getting annual FLUXNET-2015_Tier1 data...")
	  adf <-  lapply( as.list(settings_eval$sitenames_used), 
										  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
													path_fluxnet2015 = settings_eval$path_fluxnet2015, 
													timescale = "y" ) %>% 
									## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
									mutate( gpp_obs = remove_outliers( gpp_obs, coef=1.5 ),
													year = year(date),
													sitename = x ) ) %>%
	  							bind_rows() %>%
	                select(-soilm_obs_mean)

	  ##------------------------------------------------------------
	  ## Read monthly observational data from files (from monthly files!).
	  ##------------------------------------------------------------
	  ## loop over sites to get data frame with all variables
	  print("getting monthly FLUXNET-2015_Tier1 data...")
	  mdf <-  lapply( as.list(settings_eval$sitenames_used), 
										  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
													path_fluxnet2015 = settings_eval$path_fluxnet2015, 
													timescale = "m" ) %>% 
									## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
									mutate( gpp_obs = remove_outliers( gpp_obs, coef=1.5 ),
													year = year(date),
													sitename = x ) ) %>%
	  							bind_rows() %>%
	                select(-soilm_obs_mean)

	  # ##------------------------------------------------------------
	  # ## Read weekly observational data from files (from weekly files!).
	  # ##------------------------------------------------------------
	  # ## loop over sites to get data frame with all variables
	  # print("getting weekly FLUXNET-2015_Tier1 data...")
	  # wdf <-  lapply( as.list(settings_eval$sitenames_used), 
			# 							  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
			# 										path_fluxnet2015 = settings_eval$path_fluxnet2015, 
			# 										timescale = "w" ) %>% 
			# 						## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
			# 						mutate( gpp_obs = remove_outliers( gpp_obs, coef=1.5 ),
			# 										year = year(date),
			# 										sitename = x ) ) %>%
	  # 							bind_rows() %>%
	  #               select(-soilm_obs_mean)

	  ##------------------------------------------------------------
	  ## Read daily observational data from files (from daily files!).
	  ##------------------------------------------------------------
	  ## loop over sites to get data frame with all variables
	  print("getting daily FLUXNET-2015_Tier1 data...")
	  ddf <-  lapply( as.list(settings_eval$sitenames_used), 
										  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
													path_fluxnet2015 = settings_eval$path_fluxnet2015, 
													timescale = "d" ) %>% 
									## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
									mutate( gpp_obs = remove_outliers( gpp_obs, coef=1.5 ),
													year = year(date),
													sitename = x ) ) %>%
	  							bind_rows()

	  ## Add forcing data to daily data frame (for neural network-based evaluation)
	  ddf <- lapply( as.list(settings_eval$sitenames_used), function(x) get_forcing_from_csv( x, settings_sims ) ) %>%
	         bind_rows() %>%
	         select(-year_dec.x, -year_dec.y) %>%
	  			 right_join( ddf, by = c("sitename", "date") )


		# ndatapoints <- ddf %>% group_by( sitename ) %>% summarise( no = n() )  							

	  ##------------------------------------------------------------
	  ## Aggregate to multi-day periods
	  ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
	  ##------------------------------------------------------------
		# ## 8-day periods corresponding to MODIS dates (problem: doesn't start with Jan 1 each year)
  	#  breaks <- modisdates <- read_csv( "modisdates.csv" )$date

	  # ## aggregate to weeks
	  # xdf <- ddf %>% mutate( inbin = week(date) ) %>%
	  #                group_by( sitename, year, inbin ) %>%
	  #                summarise( gpp_obs = mean( gpp_obs, na.rm=TRUE) )

	  ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
		listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )	                 
		ndays <- 5
		breaks <- map( as.list(listyears), ~seq( from=., by=paste0( settings_eval$agg, " days"), length.out = ceiling(365 / settings_eval$agg)) ) %>% Reduce(c,.)
	
		## take mean across periods
		xdf <- ddf %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
		 							 group_by( sitename, inbin ) %>%
		 							 summarise( gpp_obs_mean = mean( gpp_obs, na.rm = TRUE ), gpp_obs_min = min( gpp_obs, na.rm = TRUE ), gpp_obs_max = max( gpp_obs, na.rm = TRUE ), n_obs = sum(!is.na(gpp_obs)) ) %>%
		               rename( gpp_obs = gpp_obs_mean ) %>%
		               mutate( gpp_obs = ifelse(is.nan(gpp_obs), NA, gpp_obs ), gpp_obs_min = ifelse(is.infinite(gpp_obs_min), NA, gpp_obs_min ), gpp_obs_max = ifelse(is.infinite(gpp_obs_max), NA, gpp_obs_max ) )

	  ##------------------------------------------------------------
	  ## Get daily model output
	  ##------------------------------------------------------------
	  ddf_mod <- lapply( as.list(settings_eval$sitenames_used),  function(x) dplyr::select( mod[[x]] , date, gpp_mod = gpp ) %>% mutate( sitename = x ) ) %>%
	    bind_rows()

	  ##------------------------------------------------------------
	  ## Aggregate model output data to annual/monthly/weekly, only for selected sites,
	  ## and merge into respective observational data frame 
	  ##------------------------------------------------------------
	  ## annual sum
	  adf <- ddf_mod %>%
	    mutate( year = year(date) ) %>%
	    group_by( sitename, year ) %>%
	    summarise( gpp_mod = sum(gpp_mod), n = n() ) %>%
	    mutate( gpp_mod = ifelse( n<365, NA, gpp_mod ) ) %>%
	    ## merge into observational data frame
	    right_join( adf, by = c("sitename", "year"))

	  ## monthly mean
	  mdf <- ddf_mod %>%
	    mutate( year = year(date), moy = month(date) ) %>%
	    group_by( sitename, year, moy ) %>%
	    summarise( gpp_mod = mean(gpp_mod), n = n() ) %>%
	    ## merge into observational data frame
	    right_join( mutate( mdf, moy = month(date) ), by = c("sitename", "year", "moy"))

	  # wdf <- ddf_mod %>%
	  #   mutate( year = year(date),  ) %>%
	  #   group_by( sitename, year ) %>%
	  #   summarise( gpp_mod = sum(gpp_mod), n = n() ) %>%
	  #   mutate( gpp_mod = ifelse( n<365, NA, gpp_mod ) ) %>%
	  #   ## merge into observational data frame
	  #   right_join( wdf, by = c("year", "sitename"))

	  ## mean across multi-day period
	  xdf <- ddf_mod %>% 
	  	# mutate( year = year(date), week = week(date) ) %>%
	  	mutate( year = year(date), inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
	    group_by( sitename, inbin ) %>%
	    summarise( gpp_mod_mean = mean( gpp_mod, na.rm = TRUE ), gpp_mod_min = min( gpp_mod, na.rm = TRUE ), gpp_mod_max = max( gpp_mod, na.rm = TRUE ), n_mod = sum(!is.na(gpp_mod)) ) %>%
	    rename( gpp_mod = gpp_mod_mean ) %>%
	  	right_join( xdf, by = c("sitename", "inbin") )
	  
	  ## daily
	  ddf <- ddf_mod %>%
	    ## merge into observational data frame
	    right_join( ddf, by = c("sitename", "date"))



    ##------------------------------------------------------------
	  ## Evaluate annual values by site
    ##------------------------------------------------------------
		adf_stats <- adf %>% group_by( sitename ) %>% 
												 nest() %>%
		             mutate( nyears = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ) ) %>%
		             unnest( nyears ) %>%
		             filter( nyears > 2 ) %>%
		             mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
    		                 stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
		             mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
		             unnest( stats )

    ##------------------------------------------------------------
	  ## Get mean annual GPP -> "spatial" data frame and evaluate it
    ##------------------------------------------------------------
    meandf <- adf %>% group_by( sitename ) %>%
    									summarise(  gpp_obs = mean( gpp_obs, na.rm=TRUE ),
    														  gpp_mod = mean( gpp_mod, na.rm=TRUE ) )

    linmod_meandf <- lm( gpp_obs ~ gpp_mod, data = meandf ) 
		stats_meandf  <- with( meandf, get_stats( gpp_mod, gpp_obs ) )

		# ## test if identical data to previous evaluation
		# mymeandf <- meandf
		# load("../soilm_global/meandf_soilm_global.Rdata")
		# meandf <- meandf %>% select( sitename=mysitename, gpp_obs_old=gpp_obs, gpp_mod_old=gpp_pmodel ) %>% 
		#           left_join( mymeandf, by="sitename" ) %>%
		#           mutate( diff = gpp_mod - gpp_mod_old )
		# with(meandf, plot( gpp_obs_old, gpp_obs))
		# lines(c(0,4000), c(0,4000))
		# with(meandf, plot( gpp_mod_old, gpp_mod))
		# lines(c(0,4000), c(0,4000))
		
		# ## daily values seem to be identical. something wrong with aggregating model outputs to annual values?
		# load( file="../soilm_global/data/nice_nn_agg_lue_obs_evi.Rdata" )
		# ddf_old <- nice_agg %>% select( sitename = mysitename, date, gpp_mod_old = gpp_pmodel )
		# ddf_new <- lapply( as.list(settings_eval$sitenames_used),  function(x) dplyr::select( mod[[x]] , date, gpp_mod = gpp ) %>% mutate( sitename = x ) ) %>%
		#   bind_rows() %>% left_join( ddf_old, by=c("sitename", "date"))
		# with( filter(ddf_new, sitename=="AR-Vir"), plot(gpp_mod_old, gpp_mod) )
		# with( filter(ddf_new, sitename=="AU-ASM"), plot(gpp_mod_old, gpp_mod) )
		# with( filter(ddf_new, sitename=="US-Me2"), plot(gpp_mod_old, gpp_mod) )
		# with( ddf_new, plot( gpp_mod_old, gpp_mod ) )
		
    ##------------------------------------------------------------
	  ## Get IAV as annual value minus mean by site
    ##------------------------------------------------------------
		iavdf <- adf %>% left_join( rename( meandf, gpp_mod_mean = gpp_mod, gpp_obs_mean = gpp_obs ), by = "sitename" ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											select( -gpp_obs_mean, -gpp_mod_mean )
		
		iavdf_stats <- iavdf %>% 
									  group_by( sitename ) %>%
									  nest() %>%
									  mutate( nyears = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ) ) %>%
									  unnest( nyears ) %>%
									  filter( nyears > 2 ) %>%
									  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
									          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
									  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
									  unnest( stats )

    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by day of year)
    ##------------------------------------------------------------
		meandoydf <- ddf %>%  mutate( doy = yday(date) ) %>%
		                      filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
												  mutate( gpp_obs = ifelse(is.nan(gpp_obs), NA, gpp_obs ) ) %>%
													group_by( sitename, doy ) %>% 
													summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
																		 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
																		 ) %>%
													mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

		meandoydf_stats <- meandoydf %>% group_by( sitename ) %>%
											 nest()


    ##------------------------------------------------------------
	  ## Get IDV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
		idvdf <- ddf %>%  mutate( doy = yday(date) ) %>%
		                  left_join( rename( meandoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "doy") ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
		
		idvdf_stats <- idvdf %>% 
									  group_by( sitename ) %>%
									  nest() %>%
									  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
									          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
									  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
									  unnest( stats )									  
		
    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by week (or X-day period) of year)
    ##------------------------------------------------------------
		meanxoydf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
													group_by( sitename, xoy ) %>% 
													summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
																		 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
																		 ) %>%
													mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

		meanxoydf_stats <- meanxoydf %>% group_by( sitename ) %>%
											 nest()

    ##------------------------------------------------------------
	  ## Get IXV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
		ixvdf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
		                  left_join( rename( meanxoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "xoy") ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
		
		ixvdf_stats <- ixvdf %>% 
									  group_by( sitename ) %>%
									  nest() %>%
									  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
									          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
									  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
									  unnest( stats )									  
		
    ##------------------------------------------------------------
	  ## Plot mean per site -> spatial correlation
    ##------------------------------------------------------------
		par(las=1, mar=c(4,4.5,4,1))
		stats <- with( meandf, analyse_modobs( gpp_mod, gpp_obs, heat = FALSE, col = "black", ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ) ) )
		abline( linmod_meandf, col="red")
    title( "Spatial correlation" )
    
    ##------------------------------------------------------------
    ## Combined spatial - IAV correlation
    ##------------------------------------------------------------
    par(las=1, mar=c(4,4.5,4,1))
    with( meandf, plot( gpp_mod, gpp_obs, xlim = c(0,4000), ylim = c(0,4000), pch=16, col=rgb(0,0,0,0.5), type = "n", ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ) ) )
		abline( linmod_meandf, col="red")
		out <- adf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = . ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		title( "Spatial/annual correlation" )
		
    ##------------------------------------------------------------
    ## IAV correlation: x_(y,i) - mean_y( x_(y,i) )
    ##------------------------------------------------------------
		with( iavdf, plot(gpp_mod, gpp_obs, pch = 16, col=rgb(0,0,0,0.5), xlim=c(-400,400), ylim=c(-1000,1000), ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) )  ))
		out <- iavdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(1,0,0,0.5) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		lines(c(-1000,5000), c(-1000,5000), lty=3 )
		title( "IAV correlation" )

		##------------------------------------------------------------
		## Histogram of slopes
		##------------------------------------------------------------
		## (Uncomment to plot as inset in spatial-IAV plot) 
		# u <- par("usr")
		# v <- c(
		#   grconvertX(u[1:2], "user", "ndc"),
		#   grconvertY(u[3:4], "user", "ndc")
		# )
		# v_orig <- v
		# v <- c( v[1]+0.03, v[1]+0.2*v[2], v[3]+0.50*v[4], v[3]+0.72*v[4] )
		# par( fig=v, new=TRUE, mar=c(0,0,0,0), mgp=c(3,0.5,0) )
		hist( adf_stats$slope, xlim=c(-5,5), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 50, xlab="slope" )
		abline( v=1.0, col="red" )
		axis( 1, cex.axis=1.0, xlab="slope" )
		title( "Slopes of annual regressions" )
		
		##------------------------------------------------------------
		## Histogram of R2
		##------------------------------------------------------------
		## (Uncomment to plot as inset in spatial-IAV plot) 
		# u <- par("usr")
		# v <- c(
		#   grconvertX(u[1:2], "user", "ndc"),
		#   grconvertY(u[3:4], "user", "ndc")
		# )
		# v_orig <- v
		# v <- c( v[1]+0.03, v[1]+0.2*v[2], v[3]+0.50*v[4], v[3]+0.72*v[4] )
		# par( fig=v, new=TRUE, mar=c(0,0,0,0), mgp=c(3,0.5,0) )
		hist( adf_stats$rsq, xlim=c(-1,1), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 12, xlab= bquote( italic(R)^2 ) )
		abline( v=1.0, col="red" )
		axis( 1, cex.axis=1.0, xlab = bquote( italic(R)^2 ) )
		title( bquote( bold(Slopes ~ of ~ italic(R)^2) ) )
		
    ##------------------------------------------------------------
    ## IDV correlation: x_(d,i) - mean_d( x_(d,i) )
    ##------------------------------------------------------------
		# with( idvdf, heatscatter(gpp_mod, gpp_obs, xlim=c(-15,15), ylim=c(-15,15), ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )  ))
		with( idvdf, plot(gpp_mod, gpp_obs, col=rgb(0,0,0,0.05), pch=16, xlim=c(-15,15), ylim=c(-15,15), ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )  ))
		out <- idvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(1,0,0,0.1) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		lines(c(-1000,5000), c(-1000,5000), lty=3 )
		title( "IDV correlation" )
		
		##------------------------------------------------------------
		## histogram of daily anomalies from mean seasonal cycle based on DOY
		##------------------------------------------------------------
		par(las=1)
		with( idvdf, hist( gpp_obs, breaks = 20, col = rgb(0,0,0,0.3), freq = FALSE, main = "Daily anomalies", xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ) ) )
		with( idvdf, hist( gpp_mod, breaks = 20, col = rgb(1,0,0,0.3), freq = FALSE, add = TRUE ) )
    mtext( bquote( sigma[obs] == .(format( sd(idvdf$gpp_obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 )	
    mtext( bquote( sigma[mod] == .(format( sd(idvdf$gpp_mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )	
    legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
    
		##------------------------------------------------------------
		## histogram of X-daily anomalies from mean seasonal cycle based on XOY
		##------------------------------------------------------------
		par(las=1)
		with( ixvdf, hist( gpp_obs, breaks = 20, col = rgb(0,0,0,0.3), freq = FALSE, main = "Anomalies in X-day periods", xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ), ylim = c(0,0.45) ) )
		with( ixvdf, hist( gpp_mod, breaks = 20, col = rgb(1,0,0,0.3), freq = FALSE, add = TRUE ) )
    mtext( bquote( sigma[obs] == .(format( sd(ixvdf$gpp_obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 )	
    mtext( bquote( sigma[mod] == .(format( sd(ixvdf$gpp_mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )	
    legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
    
    ##------------------------------------------------------------
    ## IXV correlation: x_(x,i) - mean_x( x_(x,i) )
    ##------------------------------------------------------------
		with( ixvdf, plot(
		  gpp_mod, 
		  gpp_obs, 
		  col=rgb(0,0,0,0.05), 
		  pch=16, 
		  xlim=c(-10,10), 
		  ylim=c(-10,10), 
		  ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
		  xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )
		  ))
		out <- ixvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = ., col=rgb(1,0,0,0.1) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		lines(c(-1000,5000), c(-1000,5000), lty=3 )
		title( "IXV correlation" )

    ##------------------------------------------------------------
    ## Mean seasonal cycle by day of year (DOY)
    ##------------------------------------------------------------
    ## observed vs. modelled
		modobs_meandoy <- with( meandoydf, analyse_modobs( mod_mean, obs_mean, heat=TRUE, ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ) ) )
		title("Mean-by-DOY correlation")
		
		## subset site selection
		purrr::map( filter( meandoydf_stats, sitename %in% settings_eval$sitenames_siteplots )$data, ~plot_by_doy_bysite(.) )
		
		# ##------------------------------------------------------------
		# ## Mean seasonal cycle by x-day-period of year (XOY)
		# ##------------------------------------------------------------
		# ## observed vs. modelled
		# modobs_meanxoy <- with( meanxoydf, analyse_modobs( mod_mean, obs_mean, heat=TRUE, ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ) ) )											   
		# title("Mean-by-XOY correlation")
		# 
		# purrr::map( filter( meanxoydf_stats, sitename %in% settings_eval$sitenames_siteplots )$data, ~plot_by_xoy_bysite(.) )

    ##------------------------------------------------------------
    ## Daily values (absolute)
    ##------------------------------------------------------------
    ## observed vs. modelled
		modobs_ddf <- with( ddf, analyse_modobs( gpp_mod, gpp_obs, heat=TRUE, ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ) ) )
		title("Correlation of daily GPP")

		##------------------------------------------------------------
		## Aggregated values (absolute) to X-day periods
		##------------------------------------------------------------
		## observed vs. modelled
		modobs_ddf <- with( xdf, analyse_modobs( gpp_mod, gpp_obs, heat=TRUE, ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ) ) )
		title("Correlation of mean GPP in X-day periods")
		
	}

}

plot_by_doy_bysite <- function( df ){
  yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
  plot(  df$doy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
  polygon( c(df$doy, rev(df$doy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
  lines( df$doy, df$mod_mean, col="red", lwd=1.75 )
  polygon( c(df$doy, rev(df$doy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
  title( df$site[1] )
}

plot_by_xoy_bysite <- function( df ){
  print(df$site[1])
  yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
  plot(  df$xoy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
  polygon( c(df$xoy, rev(df$xoy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
  lines( df$xoy, df$mod_mean, col="red", lwd=1.75 )
  polygon( c(df$xoy, rev(df$xoy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
  title( df$site[1] )
}

get_stats <- function( mod, obs ){

	linmod <- lm( obs ~ mod )
	linmod_sum <- summary( linmod )
	rsq <- linmod_sum$adj.r.squared
	rmse <- sqrt( mean( (mod - obs)^2, na.rm=TRUE ) )
	slope <- coef(linmod)[2]
	nvals <- sum( !is.na(mod) & !is.na(obs) )
	return( tibble( rsq=rsq, rmse=rmse, slope=slope, nvals=nvals ) )

}

add_fitted <- function( data ){
  linmod <- lm( gpp_obs ~ gpp_mod, data = data, na.action = "na.exclude" )
  data$fitted <- fitted( linmod )
  return(data)  
}

interpol_lin <- function(vec){
	out <- approx( seq(length(vec)), vec, xout = seq(length(vec)) )$y
	return(out)
}

get_forcing_from_csv <- function( sitename, settings_sims ){

	## get climate data
  dir <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename )
  csvfiln <- paste0( dir, "/clim_daily_", sitename, ".csv" )
  ddf <- read_csv( csvfiln )

  ## get fapar data
  dir <- paste0( settings_sims$path_input, "/sitedata/fapar/", sitename )
  csvfiln <- paste0( dir, "/fapar_daily_", sitename, ".csv" )
  ddf <- read_csv( csvfiln ) %>%
         mutate( fapar = as.numeric(fapar)) %>%
  			 right_join( ddf, by = "date" )

  return(ddf)

}

