eval_sofun <- function( mod, settings_eval, settings_sims, siteinfo, overwrite=TRUE, doplot=FALSE ){
	
  require(dplyr)
  require(purrr)
  require(lubridate)
  require(tidyr)
  require(stringr)

  source("remove_outliers.R")
  source("analyse_modobs.R")

  metrics <- list()
  
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
	  source("eval_response_neuralnet.R")

	  ## get sites for which no model output is available
	  missing_mod <- purrr::map_lgl( mod, ~identical(., NA ) ) %>% which() %>% names()
	  settings_eval$sitenames_used <- settings_eval$sitenames[which(!(settings_eval$sitenames %in% missing_mod))]
	  
	  metrics$gpp$fluxnet2015 <- list()

	  if (!file.exists("adf.Rdata")||!file.exists("ddf.Rdata")||overwrite){

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
		                dplyr::select(-soilm_obs_mean)

			  ##------------------------------------------------------------
			  ## Read monthly observational data from files (from monthly files!).
			  ##------------------------------------------------------------
			  ## loop over sites to get data frame with all variables
			  print("getting monthly FLUXNET-2015_Tier1 data...")
			  mdf <-  lapply( as.list(settings_eval$sitenames_used), 
												  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
															path_fluxnet2015 = settings_eval$path_fluxnet2015, 
															timescale = "m" ) %>% 
											mutate( year = year(date),
															sitename = x ) ) %>%
			  							bind_rows()

		  ##------------------------------------------------------------
		  ## Read daily observational data from files (from daily files!).
		  ##------------------------------------------------------------
		  ## loop over sites to get data frame with all variables
		  print("getting daily FLUXNET-2015_Tier1 data...")
		  ddf <-  lapply( as.list(settings_eval$sitenames_used), 
											  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
														path_fluxnet2015 = settings_eval$path_fluxnet2015, 
														timescale = "d" ) %>% 
										mutate( year = year(date),
														sitename = x ) ) %>%
		  							bind_rows()

		  ## Add forcing data to daily data frame (for neural network-based evaluation)
		  ddf <- lapply( as.list(settings_eval$sitenames_used), function(x) get_forcing_from_csv( x, settings_sims ) ) %>%
		         bind_rows() %>%
		         dplyr::select(-year_dec.x, -year_dec.y) %>%
		  			 right_join( ddf, by = c("sitename", "date") )


		  ##------------------------------------------------------------
		  ## Create table for overview
		  ##------------------------------------------------------------
		  filn <- "siteinfo_eval.csv"
		  if (!file.exists(filn)||overwrite){
		  	## Get additional meta information for sites: Koeppen-Geiger Class
		  	## First, get this info from a separate CSV file
		    tmp <-  read_csv("~/data/FLUXNET-2015_Tier1/meta/fluxnet_site_info_mysub.csv") %>%
		            dplyr::rename( sitename = fluxnetid ) %>% dplyr::select( sitename, koeppen_climate )
		    
			  meta <- tmp %>%
			          mutate( koeppen_climate = str_split( koeppen_climate, " - " ) ) %>%
			          mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
			          mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
			          unnest( koeppen_code )

			  ## add info: number of data points (daily GPP)
				siteinfo_eval <- ddf %>% group_by( sitename ) %>% summarise( ndailygpp = sum(!is.na(gpp_obs)) ) %>% 
														right_join( dplyr::rename( siteinfo$light, sitename = mysitename), by = "sitename" ) %>%
														left_join( meta, by = "sitename")
				
				legend <- tmp$koeppen_climate %>% as_tibble() %>% 
				  filter( !is.na(value) ) %>%
				  filter( value!="-" ) %>%
				  mutate( koeppen_climate = str_split( value, " - " ) ) %>%
				  mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
				  mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
				  unnest( koeppen_code ) %>% 
				  unnest( koeppen_word ) %>% 
				  dplyr::select( Code = koeppen_code, Climate = koeppen_word ) %>% 
				  distinct( Code, .keep_all = TRUE ) %>%
				  arrange( Code )

				write_csv( legend, path = "koeppen_legend.csv" )
				
				## Second, extract the class from a global map, complement missing in above
				require(raster)
				kgclass <- raster("~/data/koeppengeiger/koeppen-geiger.tif")
				kglegend <- read_csv("~/data/koeppengeiger/koppen-geiger_legend.csv") %>% setNames( c("kgnumber", "koeppen_code_extr"))
				siteinfo_eval <- siteinfo_eval %>% mutate( kgnumber = extract( kgclass, data.frame( x=.$lon, y=.$lat ) ) ) %>% 
				  left_join( kglegend, by = "kgnumber" ) %>%
				  mutate( koeppen_code = ifelse( is.na(koeppen_code), koeppen_code_extr, koeppen_code ) ) %>%
				  dplyr::select( -koeppen_climate, -koeppen_word )
							
				write_csv( siteinfo_eval, path = filn )
		  } else {
		    siteinfo_eval <- read_csv( filn )
		  }

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
			breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( settings_eval$agg, " days"), length.out = ceiling(365 / settings_eval$agg)) ) %>% Reduce(c,.)
		
			## take mean across periods
			xdf <- ddf %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
			 							 group_by( sitename, inbin ) %>%
			 							 summarise( gpp_obs_mean = mean( gpp_obs, na.rm = TRUE ), gpp_obs_min = min( gpp_obs, na.rm = TRUE ), gpp_obs_max = max( gpp_obs, na.rm = TRUE ), n_obs = sum(!is.na(gpp_obs)) ) %>%
			               dplyr::rename( gpp_obs = gpp_obs_mean ) %>%
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

		  ## mean across multi-day period
		  xdf <- ddf_mod %>% 
		  	# mutate( year = year(date), week = week(date) ) %>%
		  	mutate( year = year(date), inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
		    group_by( sitename, inbin ) %>%
		    summarise( gpp_mod_mean = mean( gpp_mod, na.rm = TRUE ), gpp_mod_min = min( gpp_mod, na.rm = TRUE ), gpp_mod_max = max( gpp_mod, na.rm = TRUE ), n_mod = sum(!is.na(gpp_mod)) ) %>%
		    dplyr::rename( gpp_mod = gpp_mod_mean ) %>%
		  	right_join( xdf, by = c("sitename", "inbin") )
		  
		  ## daily
		  ddf <- ddf_mod %>%
		    ## merge into observational data frame
		    right_join( ddf, by = c("sitename", "date"))

		  ## save collected observational data
	    save( adf, file = "adf.Rdata")
	    save( ddf, file = "ddf.Rdata")

	  } else {

	  	load("adf.Rdata")
	  	load("ddf.Rdata")

	  }


		## metrics for daily and x-daily values, all sites pooled
    metrics$gpp$fluxnet2015$daily_pooled  <- with( ddf, get_stats( gpp_mod, gpp_obs ) )
    metrics$gpp$fluxnet2015$xdaily_pooled <- with( xdf, get_stats( gpp_mod, gpp_obs ) )

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

		## metrics for annual values, all sites pooled
    metrics$gpp$fluxnet2015$annual_pooled <- with( adf, get_stats( gpp_mod, gpp_obs ) )

    ##------------------------------------------------------------
	  ## Evaluate annual values by site
    ##------------------------------------------------------------
		mdf_stats <- mdf %>% group_by( sitename ) %>% 
												 nest() %>%
		             mutate( nmonths = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ) ) %>%
		             unnest( nmonths ) %>%
		             filter( nmonths > 2 ) %>%
		             mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
    		                 stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
		             mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
		             unnest( stats )

		## metrics for annual values, all sites pooled
    metrics$gpp$fluxnet2015$monthly_pooled <- with( mdf, get_stats( gpp_mod, gpp_obs ) )

    ##------------------------------------------------------------
	  ## Get mean annual GPP -> "spatial" data frame and evaluate it
    ##------------------------------------------------------------
    meandf <- adf %>% group_by( sitename ) %>%
    									summarise(  gpp_obs = mean( gpp_obs, na.rm=TRUE ),
    														  gpp_mod = mean( gpp_mod, na.rm=TRUE ) )

    linmod_meandf <- lm( gpp_obs ~ gpp_mod, data = meandf ) 
    metrics$gpp$fluxnet2015$spatial <- with( meandf, get_stats( gpp_mod, gpp_obs ) )
    
		# ## test if identical data to previous evaluation
		# mymeandf <- meandf
		# load("../soilm_global/meandf_soilm_global.Rdata")
		# meandf <- meandf %>% dplyr::select( sitename=mysitename, gpp_obs_old=gpp_obs, gpp_mod_old=gpp_pmodel ) %>% 
		#           left_join( mymeandf, by="sitename" ) %>%
		#           mutate( diff = gpp_mod - gpp_mod_old )
		# with(meandf, plot( gpp_obs_old, gpp_obs))
		# lines(c(0,4000), c(0,4000))
		# with(meandf, plot( gpp_mod_old, gpp_mod))
		# lines(c(0,4000), c(0,4000))
		
		# ## daily values seem to be identical. something wrong with aggregating model outputs to annual values?
		# load( file="../soilm_global/data/nice_nn_agg_lue_obs_evi.Rdata" )
		# ddf_old <- nice_agg %>% dplyr::select( sitename = mysitename, date, gpp_mod_old = gpp_pmodel )
		# ddf_new <- lapply( as.list(settings_eval$sitenames_used),  function(x) dplyr::select( mod[[x]] , date, gpp_mod = gpp ) %>% mutate( sitename = x ) ) %>%
		#   bind_rows() %>% left_join( ddf_old, by=c("sitename", "date"))
		# with( filter(ddf_new, sitename=="AR-Vir"), plot(gpp_mod_old, gpp_mod) )
		# with( filter(ddf_new, sitename=="AU-ASM"), plot(gpp_mod_old, gpp_mod) )
		# with( filter(ddf_new, sitename=="US-Me2"), plot(gpp_mod_old, gpp_mod) )
		# with( ddf_new, plot( gpp_mod_old, gpp_mod ) )
		
    ##------------------------------------------------------------
	  ## Get IAV as annual value minus mean by site
    ##------------------------------------------------------------
		iavdf <- adf %>% left_join( dplyr::rename( meandf, gpp_mod_mean = gpp_mod, gpp_obs_mean = gpp_obs ), by = "sitename" ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, 
											        gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											dplyr::select( -gpp_obs_mean, -gpp_mod_mean )
		
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

		metrics$gpp$fluxnet2015$anomalies_annual  <- with( iavdf, get_stats( gpp_mod, gpp_obs ) )
		
    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by day of year)
    ##------------------------------------------------------------
		meandoydf <- ddf %>%  mutate( doy = yday(date) ) %>%
		                      filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
													group_by( sitename, doy ) %>% 
													summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
																		 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
																		 ) %>%
		                      mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
													mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

		meandoydf_stats <- meandoydf %>% group_by( sitename ) %>%
											 nest()

		metrics$gpp$fluxnet2015$meandoy  <- with( meandoydf, get_stats( mod_mean, obs_mean ) )

    ## aggregate mean seasonal cycle by climate zone (koeppen-geiger) and hemisphere (pooling sites within the same climate zone)
		meandoydf_byclim <- ddf %>% mutate( doy = yday(date) ) %>%
															  left_join( dplyr::select( siteinfo_eval, sitename, lat, koeppen_code ), by = "sitename" ) %>%
															  mutate( hemisphere = ifelse( lat>0, "north", "south" ) ) %>%
															  dplyr::select( -lat ) %>%
															  filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
															  group_by( koeppen_code, hemisphere, doy ) %>% 
															  summarise( obs_mean = median( gpp_obs, na.rm=TRUE ), obs_min = quantile( gpp_obs, 0.33, na.rm=TRUE ), obs_max = quantile( gpp_obs, 0.66, na.rm=TRUE ),
															             mod_mean = median( gpp_mod, na.rm=TRUE ), mod_min = quantile( gpp_mod, 0.33, na.rm=TRUE ), mod_max = quantile( gpp_mod, 0.66, na.rm=TRUE ) ) %>%
		                            mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
		                            mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ) ) %>%
		                            mutate( climatezone = paste( koeppen_code, hemisphere ) ) %>%
		                            left_join( 
		                            	(siteinfo_eval %>% mutate( hemisphere = ifelse( lat>0, "north", "south" ) ) %>% group_by( koeppen_code, hemisphere ) %>% summarise( nsites = n() )), 
		                            	by = c("koeppen_code", "hemisphere") 
		                            	)

		meandoydf_byclim_stats <- meandoydf_byclim %>% group_by( koeppen_code, hemisphere ) %>%
															nest()
		  
    ##------------------------------------------------------------
	  ## Get IDV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
		idvdf <- ddf %>%  mutate( doy = yday(date) ) %>%
		                  left_join( dplyr::rename( meandoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "doy") ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											dplyr::select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
		
		idvdf_stats <- idvdf %>% 
									  group_by( sitename ) %>%
									  nest() %>%
									  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
									          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
									  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
									  unnest( stats )									  
		
		metrics$gpp$fluxnet2015$anomalies_daily  <- with( idvdf, get_stats( gpp_mod, gpp_obs ) )
		
    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by week (or X-day period) of year)
    ##------------------------------------------------------------
		meanxoydf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
													group_by( sitename, xoy ) %>% 
													summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
																		 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
																		 ) %>%
		                      mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
													mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

		meanxoydf_stats <- meanxoydf %>% group_by( sitename ) %>%
											 nest()

		metrics$gpp$fluxnet2015$meanxoy  <- with( meanxoydf, get_stats( mod_mean, obs_mean ) )

    ##------------------------------------------------------------
	  ## Get IXV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
		ixvdf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
		                  left_join( dplyr::rename( meanxoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "xoy") ) %>%
											mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
											dplyr::select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
		
		ixvdf_stats <- ixvdf %>% 
									  group_by( sitename ) %>%
									  nest() %>%
									  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
									          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
									  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
									  unnest( stats )
		
		metrics$gpp$fluxnet2015$anomalies_xdaily  <- with( ixvdf, get_stats( gpp_mod, gpp_obs ) )
		

    ##------------------------------------------------------------
	  ## Plotting
    ##------------------------------------------------------------
    if (doplot){
			modobs_ddf <- plot_modobs_daily( ddf, makepdf=FALSE )
			modobs_xdf <- plot_modobs_xdaily( xdf, makepdf=FALSE )
			modobs_mdf <- plot_modobs_monthly( mdf, makepdf=FALSE )
	    modobs_spatial <- plot_modobs_spatial( meandf, makepdf=TRUE )
			plot_modobs_spatial_annual( meandf, linmod_meandf, adf_stats, makepdf=FALSE )
			modobs_anomalies_annual <- plot_modobs_anomalies_annual( iavdf, iavdf_stats, makepdf=FALSE )
		  modobs_anomalies_daily <- plot_modobs_anomalies_daily( idvdf, idvdf_stats, makepdf=FALSE)
	  	modobs_anomalies_xdaily <- plot_modobs_anomalies_xdaily( ixvdf, ixvdf_stats, makepdf=FALSE )
	  	modobs_meandoy <- plot_modobs_meandoy( meandoydf, meandoydf_stats, makepdf=FALSE )
			plot_by_doy_allsites( meandoydf_stats, makepdf=FALSE )
			plot_by_doy_allzones( meandoydf_byclim_stats, makepdf=FALSE )
			modobs_meanxoy <- plot_modobs_meanxoy( meanxoydf, makepdf=FALSE )
			plot_by_xoy_allsites( meanxoydf_stats, makepdf=FALSE )
    }


	}
  
  data = list(  
  	adf_stats              = adf_stats,
  	mdf_stats              = mdf_stats,
    meandf                 = meandf, 
    meandf                 = meandf, 
    linmod_meandf          = linmod_meandf, 
    iavdf                  = iavdf, 
    iavdf_stats            = iavdf_stats, 
    idvdf                  = idvdf, 
    idvdf_stats            = idvdf_stats, 
    ixvdf                  = ixvdf, 
    ixvdf_stats            = ixvdf_stats, 
    meandoydf              = meandoydf, 
    meandoydf_stats        = meandoydf_stats, 
    meandoydf_stats        = meandoydf_stats, 
    meandoydf_byclim_stats = meandoydf_byclim_stats, 
    meanxoydf              = meanxoydf, 
    meanxoydf_stats        = meanxoydf_stats,
    adf                    = adf,
    mdf                    = mdf,
    ddf                    = ddf, 
    xdf                    = xdf
  )
  
	return( list( metrics=metrics, data=data ) )
}

##------------------------------------------------------------
## Plot mean per site -> spatial correlation
##------------------------------------------------------------
modobs_spatial <- plot_modobs_spatial <- function( meandf, makepdf=TRUE ){
	source("analyse_modobs.R")
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
  require(purrr)
  if (makepdf) pdf("fig/modobs_spatial_annual.pdf")
    par(las=1, mar=c(4,4.5,4,1))
    with( meandf, plot( gpp_mod, gpp_obs, xlim = c(0,4000), ylim = c(0,4000), pch=16, col=rgb(0,0,0,0.5), type = "n", ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ) ) )
		abline( linmod_meandf, col="red")
		out <- adf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ gpp_mod, data = . ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., gpp_mod ) ) )
		title( "Spatial/annual correlation" )
	if (makepdf) dev.off()

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
	if (makepdf) pdf("fig/hist_slopes_anomalies_annual.pdf")
		hist( adf_stats$slope, xlim=c(-5,5), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 50, xlab="slope" )
		abline( v=1.0, col="red" )
		axis( 1, cex.axis=1.0, xlab="slope" )
		title( "Slopes of annual regressions" )
	if (makepdf) dev.off()

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
	if (makepdf) pdf("fig/hist_r2_anomalies_annual.pdf")
		hist( adf_stats$rsq, xlim=c(-1,1), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 12, xlab= bquote( italic(R)^2 ) )
		abline( v=1.0, col="red" )
		axis( 1, cex.axis=1.0, xlab = bquote( italic(R)^2 ) )
		title( bquote( bold(Slopes ~ of ~ italic(R)^2) ) )
	if (makepdf) dev.off()
	
}


##------------------------------------------------------------
## IAV correlation: x_(y,i) - mean_y( x_(y,i) )
##------------------------------------------------------------
plot_modobs_anomalies_annual <- function( iavdf, iavdf_stats, makepdf=FALSE ){
	source("analyse_modobs.R")
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
	source("analyse_modobs.R")
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
	source("analyse_modobs.R")
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
	source("analyse_modobs.R")
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
	mylist <- read_csv("myselect_fluxnet2015.csv") %>% filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
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
	source("analyse_modobs.R")
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
	mylist <- read_csv("myselect_fluxnet2015.csv") %>% filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
	tmp <- purrr::map( filter( meanxoydf_stats, sitename %in% mylist )$data, ~plot_by_xoy_bysite(., makepdf = TRUE ) )
}


##------------------------------------------------------------
## Daily values (absolute)
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_daily <- function( ddf, makepdf=FALSE, ... ){
	source("analyse_modobs.R")
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
	source("analyse_modobs.R")
	if (makepdf) pdf("fig/modobs_monthly.pdf")
	modobs_mdf <- with( mdf, 
		analyse_modobs( 
			gpp_mod, 
			gpp_obs, 
			heat=TRUE, 
			ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
			xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
			plot.title = "Correlation of monthly GPP",
			...
		) )
	if (makepdf) dev.off()
	return( modobs_mdf )
}


##------------------------------------------------------------
## Aggregated values (absolute) to X-day periods
##------------------------------------------------------------
## observed vs. modelled
plot_modobs_xdaily <- function( xdf, makepdf=FALSE, ... ){
	source("analyse_modobs.R")
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

extract_koeppen_code <- function( str ){
	require(stringr)
	out <- str_split( str, " - ")[[1]][1]
	return( out )
}

