#' Evaluates SOFUN model outputs.
#'
#' Calculates a set of perfomance metrics for model outputs, compared against observational data.
#' Currently only evaluations of GPP model outputs, compared agains FLUXNET 2015 data, are implemented.
#'
#' @param settings A named list of data frames containing containing model outputs. 
#' The names of list elements corresponds to site names.
#' @param settings_eval A list specifying evaluation settings 
#' (see vignette eval_sofun.pdf for more information and examples)
#' @param settings_sims A named list containing the simulation settings 
#' (see vignette_rsofun.pdf for more information and examples)
#' @param obs_eval (Optional) A named list of data frames containing observational data for each sites. 
#' The names of list elements corresponds to site names. Defaults to \code{NA} 
#' @param overwrite (Optional) A logical specifying whether temporary data stored in \code{./tmpdir} should be overwritten. Defaults to \code{TRUE}.
#'
#' @return A list containing data frames of modelled and observed values aggregated to several temporal scales 
#' (ddf for daily, xdf for X-daily, mdf for monthly, adf for annual), and respective performance metrics.
#' @export
#'
#' @examples out_eval <- eval_sofun( mod, settings_eval, settings_sims, obs_eval = NA, overwrite = TRUE, doplot = FALSE )
#' 
eval_sofun <- function( mod, settings_eval, settings_sims, obs_eval = NA, overwrite = TRUE, doplot = FALSE ){

  metrics <- list()

  datasource <- str_split( settings_eval$benchmark$gpp, "_" ) %>% unlist()

	if ("fluxnet2015" %in% datasource){
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

	  ## get sites for which no model output is available and overwrite settings_eval$sitenames
	  missing_mod <- purrr::map_lgl( mod, ~identical(., NA ) ) %>% which() %>% names()
	  settings_eval$sitenames <- settings_eval$sitenames[which(!(settings_eval$sitenames %in% missing_mod))]
	  
	  metrics$gpp$fluxnet2015 <- list()

	  ##------------------------------------------------------------
	  ## Get daily model output
	  ##------------------------------------------------------------
	  ddf_mod <- lapply( as.list(settings_eval$sitenames),  function(x) dplyr::select( mod$daily[[x]], date, gpp_mod = gpp ) %>% mutate( sitename = x ) ) %>%
	    bind_rows()

    ##------------------------------------------------------------
    ## Get observations for evaluation
    ##------------------------------------------------------------
	  if (identical(obs_eval, NA)) obs_eval <- get_obs_eval( settings_eval = settings_eval, settings_sims = settings_sims, overwrite = overwrite )
    
    ##------------------------------------------------------------
    ## Aggregate model output data to annual/monthly/weekly, only for selected sites,
    ## and merge into respective observational data frame 
    ##------------------------------------------------------------
	  print("Aggregating model outputs...")

    ## annual sum
    obs_eval$adf <- ddf_mod %>%
      mutate( year = year(date) ) %>%
      group_by( sitename, year ) %>%
      summarise( gpp_mod = sum(gpp_mod), n = n() ) %>%
      mutate( gpp_mod = ifelse( n<365, NA, gpp_mod ) ) %>%
      ## merge into observational data frame
      right_join( obs_eval$adf, by = c("sitename", "year"))

    ## monthly mean
    obs_eval$mdf <- ddf_mod %>%
      mutate( year = year(date), moy = month(date) ) %>%
      group_by( sitename, year, moy ) %>%
      summarise( gpp_mod = mean(gpp_mod), n = n() ) %>%
      ## merge into observational data frame
      right_join( mutate( obs_eval$mdf, moy = month(date) ), by = c("sitename", "year", "moy"))

    ## mean across multi-day period
    obs_eval$xdf <- ddf_mod %>% 
    	# mutate( year = year(date), week = week(date) ) %>%
    	mutate( year = year(date), inbin = cut( date, breaks = obs_eval$breaks_xdf, right = FALSE ) ) %>%
      group_by( sitename, inbin ) %>%
      summarise( gpp_mod_mean = mean( gpp_mod, na.rm = TRUE ), gpp_mod_min = min( gpp_mod, na.rm = TRUE ), gpp_mod_max = max( gpp_mod, na.rm = TRUE ), n_mod = sum(!is.na(gpp_mod)) ) %>%
      dplyr::rename( gpp_mod = gpp_mod_mean ) %>%
    	right_join( obs_eval$xdf, by = c("sitename", "inbin") )
    
    ## daily
    obs_eval$ddf <- ddf_mod %>%
      ## merge into observational data frame
      right_join( obs_eval$ddf, by = c("sitename", "date"))

    # ##------------------------------------------------------------
    # ## Create table for overview
    # ##------------------------------------------------------------
    # filn <- "siteinfo_eval.csv"
    # if (!file.exists(filn)||overwrite){
    # 	## Get additional meta information for sites: Koeppen-Geiger Class
    # 	## First, get this info from a separate CSV file
    #   ## XXX Joan: Put this file on Zenodo and get this file along with R-package-related data download. 
    #   tmp <-  read_csv("~/data/FLUXNET-2015_Tier1/meta/fluxnet_site_info_mysub.csv") %>%
    #           dplyr::rename( sitename = fluxnetid ) %>% dplyr::select( sitename, koeppen_climate )
      
  	 #  meta <- tmp %>%
  	 #          mutate( koeppen_climate = str_split( koeppen_climate, " - " ) ) %>%
  	 #          mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
  	 #          mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
  	 #          unnest( koeppen_code )

  	 #  ## add info: number of data points (daily GPP)
  		# siteinfo_eval <- obs_eval$ddf %>% group_by( sitename ) %>% summarise( ndailygpp = sum(!is.na(gpp_obs)) ) %>% 
  		# 	right_join( dplyr::rename( siteinfo$light, sitename = mysitename ), by = "sitename" ) %>%
  		# 	left_join( meta, by = "sitename")
  		
  		# legend <- tmp$koeppen_climate %>% as_tibble() %>% 
  		#   filter( !is.na(value) ) %>%
  		#   filter( value!="-" ) %>%
  		#   mutate( koeppen_climate = str_split( value, " - " ) ) %>%
  		#   mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
  		#   mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
  		#   unnest( koeppen_code ) %>% 
  		#   unnest( koeppen_word ) %>% 
  		#   dplyr::select( Code = koeppen_code, Climate = koeppen_word ) %>% 
  		#   distinct( Code, .keep_all = TRUE ) %>%
  		#   arrange( Code )

  		# write_csv( legend, path = "koeppen_legend.csv" )
  		
  		# ## Second, extract the class from a global map, complement missing in above
  		# kgclass <- raster("~/data/koeppengeiger/koeppen-geiger.tif")
  		# kglegend <- read_csv("~/data/koeppengeiger/koppen-geiger_legend.csv") %>% setNames( c("kgnumber", "koeppen_code_extr"))
  		# siteinfo_eval <- siteinfo_eval %>% mutate( kgnumber = extract( kgclass, data.frame( x=.$lon, y=.$lat ) ) ) %>% 
  		#   left_join( kglegend, by = "kgnumber" ) %>%
  		#   mutate( koeppen_code = ifelse( is.na(koeppen_code), koeppen_code_extr, koeppen_code ) ) %>%
  		#   dplyr::select( -koeppen_climate, -koeppen_word )
  					
  		# write_csv( siteinfo_eval, path = filn )

    # } else {

    #   siteinfo_eval <- read_csv( filn )

    # }

		## metrics for daily and x-daily values, all sites pooled
    metrics$gpp$fluxnet2015$daily_pooled <- with( obs_eval$ddf, get_stats( gpp_mod, gpp_obs ) )
    metrics$gpp$fluxnet2015$xdaily_pooled <- with( obs_eval$xdf, get_stats( gpp_mod, gpp_obs ) )

    ##------------------------------------------------------------
	  ## Evaluate annual values by site
    ##------------------------------------------------------------
    if (sum(!is.na(obs_eval$adf$gpp_obs))>2){
	    print("Evaluate annual values...")
			adf_stats <- obs_eval$adf %>% group_by( sitename ) %>% 
									 nest() %>%
			             mutate( nyears_obs = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ) ) %>%
			             unnest( nyears_obs ) %>%
			             filter( nyears_obs > 2 ) %>%
			             mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
	    		                 stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
			             mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
			             unnest( stats )

			## metrics for annual values, all sites pooled
	    metrics$gpp$fluxnet2015$annual_pooled <- with( obs_eval$adf, get_stats( gpp_mod, gpp_obs ) )
    } else {
    	adf_stats <- NA
    	metrics$gpp$fluxnet2015$annual_pooled <- list( rsq=NA, rmse=NA )
    }

    ##------------------------------------------------------------
	  ## Evaluate monthly values by site
    ##------------------------------------------------------------
    if (sum(!is.na(obs_eval$mdf$gpp_obs))>2){
	    print("Evaluate monthly values...")

	    # ## xxx debug
	    # test <- obs_eval$mdf %>% group_by( sitename ) %>% 
	    #   nest() %>%
	    #   mutate( nmonths = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ) ) %>%
	    #   unnest( nmonths ) %>%
	    #   filter( nmonths > 2 )
	    
	    # for (i in seq(157)){ 
	    #   print(test$sitename[[i]])
	    #   linmod <- lm( gpp_obs ~ gpp_mod, data = test$data[[i]] ) 
	    #   }    
	    
			mdf_stats <- obs_eval$mdf %>% group_by( sitename ) %>% 
									 nest() %>%
			             mutate( nmonths_obs = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ),
			             				 nmonths_mod = purrr::map( data, ~sum(!is.na( .$gpp_mod )  ) ) ) %>%
			             unnest( nmonths_obs, nmonths_mod ) %>%
			             filter( nmonths_obs > 2 & nmonths_mod > 0 ) %>%
			             mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = ., na.action = na.exclude ) ),
	    		                 stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
			             mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
			             unnest( stats )

			## metrics for annual values, all sites pooled
	    metrics$gpp$fluxnet2015$monthly_pooled <- with( obs_eval$mdf, get_stats( gpp_mod, gpp_obs ) )
	  } else {
	  	mdf_stats <- NA
	  	metrics$gpp$fluxnet2015$monthly_pooled <- list( rsq=NA, rmse=NA )
	  }

    ##------------------------------------------------------------
	  ## Get mean annual GPP -> "spatial" data frame and evaluate it
    ##------------------------------------------------------------
	  if (sum(!is.na(obs_eval$adf$gpp_obs))>2){
	    print("Evaluate spatial values...")
	    meandf <- obs_eval$adf %>% group_by( sitename ) %>%
								summarise(  gpp_obs = mean( gpp_obs, na.rm=TRUE ),
													  gpp_mod = mean( gpp_mod, na.rm=TRUE ) )

	    linmod_meandf <- lm( gpp_obs ~ gpp_mod, data = meandf ) 
	    metrics$gpp$fluxnet2015$spatial <- with( meandf, get_stats( gpp_mod, gpp_obs ) )
	    
			# ## test if identical data to previous evaluation
			# mymeandf <- meandf
			# load("tmpdir/../soilm_global/meandf_soilm_global.Rdata")
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
			# ddf_new <- lapply( as.list(settings_eval$sitenames),  function(x) dplyr::select( mod[[x]] , date, gpp_mod = gpp ) %>% mutate( sitename = x ) ) %>%
			#   bind_rows() %>% left_join( ddf_old, by=c("sitename", "date"))
			# with( filter(ddf_new, sitename=="AR-Vir"), plot(gpp_mod_old, gpp_mod) )
			# with( filter(ddf_new, sitename=="AU-ASM"), plot(gpp_mod_old, gpp_mod) )
			# with( filter(ddf_new, sitename=="US-Me2"), plot(gpp_mod_old, gpp_mod) )
			# with( ddf_new, plot( gpp_mod_old, gpp_mod ) )
	  } else {
	  	meandf <- NA
	  	metrics$gpp$fluxnet2015$spatial <- list( rsq=NA, rmse=NA )
	  	linmod_meandf <- NA
	  }
		
    ##------------------------------------------------------------
	  ## Get IAV as annual value minus mean by site
    ##------------------------------------------------------------
	  if (sum(!is.na(obs_eval$adf$gpp_obs))>2){
	    print("Evaluate interannual variability...")
			iavdf <- obs_eval$adf %>% left_join( dplyr::rename( meandf, gpp_mod_mean = gpp_mod, gpp_obs_mean = gpp_obs ), by = "sitename" ) %>%
								mutate( gpp_mod = gpp_mod - gpp_mod_mean, 
								        gpp_obs = gpp_obs - gpp_obs_mean ) %>%
								dplyr::select( -gpp_obs_mean, -gpp_mod_mean )
			
			iavdf_stats <- iavdf %>% 
										  group_by( sitename ) %>%
										  nest() %>%
										  mutate( nyears_obs = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ), nyears_mod = purrr::map( data, ~sum(!is.na( .$gpp_mod )  ) ) ) %>%
										  unnest( nyears_obs, nyears_mod ) %>%
										  filter( nyears_obs > 2 & nyears_mod > 2 ) %>%
										  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
										          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
										  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
										  unnest( stats )

			metrics$gpp$fluxnet2015$anomalies_annual <- with( iavdf, get_stats( gpp_mod, gpp_obs ) )
		} else {
			iavdf <- NA
			iavdf_stats <- NA
			metrics$gpp$fluxnet2015$anomalies_annual <- list( rsq=NA, rmse=NA )
		}
		
    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by day of year)
    ##------------------------------------------------------------
		if (sum(!is.na(obs_eval$ddf$gpp_obs))>2){
			print("Evaluate mean seasonal cycle...")
			meandoydf <- obs_eval$ddf %>%  mutate( doy = yday(date) ) %>%
		                filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
										group_by( sitename, doy ) %>% 
										summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
															 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
															 ) %>%
		                mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
										mutate( obs_mean_plot = interpol_lin(obs_mean), obs_min_plot = interpol_lin(obs_min), obs_max_plot = interpol_lin( obs_max ), site=sitename )

			meandoydf_stats <- meandoydf %>% group_by( sitename ) %>%
												 nest()

			metrics$gpp$fluxnet2015$meandoy <- with( meandoydf, get_stats( mod_mean, obs_mean ) )

	    ## aggregate mean seasonal cycle by climate zone (koeppen-geiger) and hemisphere (pooling sites within the same climate zone)
			print("Evaluate mean seasonal cycle by climate zones...")
			meandoydf_byclim <- obs_eval$ddf %>% mutate( doy = yday(date) ) %>%
												  left_join( dplyr::select( metainfo_Tier1_sites_kgclimate_fluxnet2015, sitename, lat, koeppen_code ), by = "sitename" ) %>%   # 'metainfo_Tier1_sites_kgclimate_fluxnet2015' is lazy-loaded with library(rsofun)
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
	                        	(metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% mutate( hemisphere = ifelse( lat>0, "north", "south" ) ) %>% group_by( koeppen_code, hemisphere ) %>% summarise( nsites = n() )), 
	                        	by = c("koeppen_code", "hemisphere") 
	                        	)

			meandoydf_byclim_stats <- meandoydf_byclim %>% 
			                          group_by( koeppen_code, hemisphere ) %>%
																nest() %>%
															  mutate( linmod = purrr::map( data, ~lm( obs_mean ~ mod_mean, data = . ) ),
															          stats  = purrr::map( data, ~get_stats( .$mod_mean, .$obs_mean ) ) ) %>%
															  mutate( data   = purrr::map( data, ~add_fitted_alternativenames(.) ) ) %>%
															  unnest( stats )
			
			metrics$gpp$fluxnet2015$meandoy_byclim <- meandoydf_byclim_stats %>% dplyr::select( -data, -linmod )
		} else {
			meandoydf <- NA 
			meandoydf_stats <- NA 
			metrics$gpp$fluxnet2015$meandoy <- list( rsq=NA, rmse=NA )
			meandoydf_byclim <- NA 
			meandoydf_byclim_stats <- NA 
			metrics$gpp$fluxnet2015$meandoy_byclim <- list( rsq=NA, rmse=NA )
		}


    ##------------------------------------------------------------
	  ## Get IDV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
    if (sum(!is.na(obs_eval$ddf$gpp_obs))>2){
			print("Evaluate inter-day variability...")
			idvdf <- obs_eval$ddf %>%  mutate( doy = yday(date) ) %>%
		            left_join( dplyr::rename( meandoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "doy") ) %>%
								mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
								dplyr::select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
			
			idvdf_stats <- idvdf %>% 
										  group_by( sitename ) %>%
										  nest() %>%
										  mutate( ndays_obs = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ), ndays_mod = purrr::map( data, ~sum(!is.na( .$gpp_mod )  ) ) ) %>%
										  unnest( ndays_obs, ndays_mod ) %>%
										  filter( ndays_obs > 2 & ndays_mod > 2 ) %>%
										  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
										          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
										  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
										  unnest( stats )									  
			
			metrics$gpp$fluxnet2015$anomalies_daily <- with( idvdf, get_stats( gpp_mod, gpp_obs ) )
		} else {
			idvdf <- NA
			idvdf_stats <- NA
			metrics$gpp$fluxnet2015$anomalies_daily <- list( rsq=NA, rmse=NA )
		}
		
    ##------------------------------------------------------------
	  ## Get mean seasonal cycle (by week (or X-day period) of year)
    ##------------------------------------------------------------
		if (sum(!is.na(obs_eval$xdf$gpp_obs))>2){
			print("Evaluate mean seasonal cycle by X-day periods...")
			meanxoydf <- obs_eval$xdf %>%  mutate( xoy = yday(inbin) ) %>%
										group_by( sitename, xoy ) %>% 
										summarise( obs_mean = mean( gpp_obs, na.rm=TRUE ), obs_min = min( gpp_obs, na.rm=TRUE ), obs_max = max( gpp_obs, na.rm=TRUE ),
															 mod_mean = mean( gpp_mod, na.rm=TRUE ), mod_min = min( gpp_mod, na.rm=TRUE ), mod_max = max( gpp_mod, na.rm=TRUE )
															 ) %>%
	                  mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
										mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

			meanxoydf_stats <- meanxoydf %>% group_by( sitename ) %>%
												 nest()

			metrics$gpp$fluxnet2015$meanxoy <- with( meanxoydf, get_stats( mod_mean, obs_mean ) )
		} else {
			meanxoydf <- NA
			meanxoydf_stats <- NA
			metrics$gpp$fluxnet2015$meanxoy <- list( rsq=NA, rmse=NA )
		}

    ##------------------------------------------------------------
	  ## Get IXV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
		if (sum(!is.na(obs_eval$xdf$gpp_obs))>2){
			print("Evaluate inter-X-day variability...")
			ixvdf <- obs_eval$xdf %>%  mutate( xoy = yday(inbin) ) %>%
	              left_join( dplyr::rename( meanxoydf, gpp_mod_mean = mod_mean, gpp_obs_mean = obs_mean ), by = c("sitename", "xoy") ) %>%
								mutate( gpp_mod = gpp_mod - gpp_mod_mean, gpp_obs = gpp_obs - gpp_obs_mean ) %>%
								dplyr::select( -gpp_obs_mean, -gpp_mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
			
			ixvdf_stats <- ixvdf %>% 
										  group_by( sitename ) %>%
										  nest() %>%
										  mutate( nxdays_obs = purrr::map( data, ~sum(!is.na( .$gpp_obs )  ) ), nxdays_mod = purrr::map( data, ~sum(!is.na( .$gpp_mod )  ) ) ) %>%
										  unnest( nxdays_obs, nxdays_mod ) %>%
										  filter( nxdays_obs > 2 & nxdays_mod > 2 ) %>%
										  mutate( linmod = purrr::map( data, ~lm( gpp_obs ~ gpp_mod, data = . ) ),
										          stats  = purrr::map( data, ~get_stats( .$gpp_mod, .$gpp_obs ) ) ) %>%
										  mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
										  unnest( stats )
			
			metrics$gpp$fluxnet2015$anomalies_xdaily <- with( ixvdf, get_stats( gpp_mod, gpp_obs ) )
		} else {
			ixvdf <- NA
			ixvdf_stats <- NA
			metrics$gpp$fluxnet2015$anomalies_xdaily <- list( rsq=NA, rmse=NA )
		}

		print("Done with eval_sofun().")
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
  	meandoydf_byclim       = meandoydf_byclim,
  	meandoydf_byclim_stats = meandoydf_byclim_stats, 
    meanxoydf              = meanxoydf, 
    meanxoydf_stats        = meanxoydf_stats,
    adf                    = obs_eval$adf,
    mdf                    = obs_eval$mdf,
    ddf                    = obs_eval$ddf, 
    xdf                    = obs_eval$xdf
  )
  
	return( list( metrics=metrics, data=data ) )
}

add_fitted <- function( data ){
  linmod <- lm( gpp_obs ~ gpp_mod, data = data, na.action = "na.exclude" )
  data$fitted <- fitted( linmod )
  return(data)  
}

add_fitted_alternativenames <- function( data ){
  linmod <- lm( obs_mean ~ mod_mean, data = data, na.action = "na.exclude" )
  data$fitted <- fitted( linmod )
  return(data)  
}

interpol_lin <- function(vec){
	out <- try( approx( seq(length(vec)), vec, xout = seq(length(vec)) )$y )
	if (class(out)=="try-error") out <- vec * NA
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
	out <- str_split( str, " - ")[[1]][1]
	return( out )
}

