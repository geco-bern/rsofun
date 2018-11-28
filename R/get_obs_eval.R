#' Get observational data for model evaluation
#'
#' Gets observational data for model evaluation, given the benchmarking variable, and data source used 
#' for the evaluation. This information is specified in the evaluation settings (argument \code{settings_eval}).
#'
#' @param settings_eval A list specifying evaluation settings (see vignette eval_sofun.pdf for more information and examples)
#' @param settings_sims A named list containing the simulation settings (see vignette_rsofun.pdf for more information and examples)
#' @param overwrite (Optional) A logical specifying whether temporary data stored in \code{./tmpdir} should be overwritten. Defaults to \code{TRUE}.
#'
#' @return A list containing data frames of observed values aggregated to several temporal scales (ddf for daily, xdf for X-daily, mdf for monthly, adf for annual).
#' @export
#'
#' @examples obs <- get_obs_eval( settings_eval, settings_sims, overwrite = TRUE )
#' 
get_obs_eval <- function( settings_eval, settings_sims, overwrite = TRUE ){

	if ("gpp" %in% names(settings_eval$benchmark)){
		##------------------------------------------------------------
		## Get observational GPP data
		##------------------------------------------------------------

		## Interpret benchmarking data specification
		datasource <- stringr::str_split( settings_eval$benchmark$gpp, "_" ) %>% unlist()

	  if ("fluxnet2015" %in% datasource){
		  ##------------------------------------------------------------
		  ## Read annual observational data from FLUXNET 2015 files (from annual files!).
		  ##------------------------------------------------------------
		  ## loop over sites to get data frame with all variables
		  if (settings_eval$path_fluxnet2015_y!=""){
			  print("getting annual FLUXNET-2015_Tier1 data...")
		    print(settings_eval$path_fluxnet2015_y)
			  adf <-  lapply( as.list(settings_eval$sitenames),
												  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
															path_fluxnet2015 = settings_eval$path_fluxnet2015_y,
															timescale = "y", method = datasource[ -which( datasource=="fluxnet2015" ) ] ) %>%
											## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
											mutate( gpp_obs = remove_outliers( gpp_obs, coef=1.5 ),
															year = year(date),
															sitename = x ) ) %>%
			  							bind_rows() %>%
			                # dplyr::select(-soilm_obs_mean) %>%
			                mutate( gpp_obs = ifelse( year < 2000, NA, gpp_obs ) ) # remove pre-modis data
      } else {
      	rlang::warn("settings_eval$path_fluxnet2015_y is empty")
			  adf <-  lapply( as.list(settings_eval$sitenames),
												  	function(x) init_dates_dataframe( 
												  		year(settings_sims$date_start[[x]]), 
												  		year(settings_sims$date_end[[x]]), 
												  		noleap = TRUE, 
												  		freq = "years" ) %>%
											## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
											mutate( gpp_obs = NA,
															year = year(date),
															sitename = x ) ) %>%
			  							bind_rows() %>%
			                mutate( gpp_obs = ifelse( year < 2000, NA, gpp_obs ) ) # remove pre-modis data
      }

			##------------------------------------------------------------
			## Read monthly observational data from FLUXNET 2015 files (from monthly files!).
			##------------------------------------------------------------
			## loop over sites to get data frame with all variables
		  if (settings_eval$path_fluxnet2015_m!=""){
				print("getting monthly FLUXNET-2015_Tier1 data...")
				mdf <-  lapply( as.list(settings_eval$sitenames),
												  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
															path_fluxnet2015 = settings_eval$path_fluxnet2015_m,
															timescale = "m", method = datasource[ -which( datasource=="fluxnet2015" ) ] ) %>%
											mutate( year = year(date),
															sitename = x ) ) %>%
											bind_rows() %>%
											mutate( gpp_obs = ifelse( date < "2000-02-18", NA, gpp_obs ) ) # remove pre-modis data
      } else {
      	rlang::warn("settings_eval$path_fluxnet2015_m is empty")
				mdf <-  lapply( as.list(settings_eval$sitenames),
												  	function(x) init_dates_dataframe( 
												  		year(settings_sims$date_start[[x]]), 
												  		year(settings_sims$date_end[[x]]), 
												  		noleap = TRUE, 
												  		freq = "months" ) %>%
											mutate( year = year(date),
															sitename = x ) ) %>%
											bind_rows() %>%
											mutate( gpp_obs = NA ) # remove pre-modis data
      }

			##------------------------------------------------------------
			## Read daily observational data from FLUXNET 2015 files (from daily files!).
			##------------------------------------------------------------
		  if (settings_eval$path_fluxnet2015_d!=""){
				## loop over sites to get data frame with all variables
				print("getting daily FLUXNET-2015_Tier1 data...")
				ddf <-  lapply( as.list(settings_eval$sitenames),
												  	function(x) get_obs_bysite_gpp_fluxnet2015( x,
															path_fluxnet2015 = settings_eval$path_fluxnet2015_d,
															timescale = "d", method = datasource[ -which( datasource=="fluxnet2015" ) ] ) %>%
											mutate( year = year(date),
															sitename = x ) ) %>%
											bind_rows() %>%
											mutate( gpp_obs = ifelse( date < "2000-02-18", NA, gpp_obs ) ) # remove pre-modis data
      } else {
      	rlang::warn("settings_eval$path_fluxnet2015_d is empty")
				ddf <-  lapply( as.list(settings_eval$sitenames),
								  	function(x) init_dates_dataframe( 
								  		year(settings_sims$date_start[[x]]), 
								  		year(settings_sims$date_end[[x]]), 
								  		noleap = TRUE, 
								  		freq = "days" ) %>%
							mutate( year = year(date),
											sitename = x ) ) %>%
							bind_rows() %>%
							mutate( gpp_obs = NA ) # remove pre-modis data
      }

			##------------------------------------------------------------
			## Read daily observational data from GEPISAT files (only daily files!).
			##------------------------------------------------------------
			if ("Ty" %in% datasource){
				ddf_gepisat <- lapply( as.list(settings_eval$sitenames),
												function(x) get_obs_bysite_gpp_gepisat( x, 
												  settings_eval$path_gepisat_d, 
													timescale = "d" ) )
				names(ddf_gepisat) <- settings_eval$sitenames

				missing_gepisat <- purrr::map_lgl( ddf_gepisat, ~identical(., NULL ) ) %>% which() %>% names()
				settings_eval$sitenames_gepisat <- settings_eval$sitenames[which(!(settings_eval$sitenames %in% missing_gepisat))]
        
				## Convert to one long data frame and add sitename to data frames inside the list
				ddf_gepisat <- ddf_gepisat %>% bind_rows( .id = "sitename" ) %>%
				               mutate( gpp_obs = ifelse( date < "2000-02-18", NA, gpp_obs ) ) %>%  # remove pre-modis data
				               dplyr::rename( gpp_obs_gepisat = gpp_obs )

        ## add to other data frames (ddf, adf, mdf) and take take weighted average for updated 'gpp_obs'
				if (!is.null(ddf_gepisat)){
				  ddf <- ddf_gepisat %>% right_join( ddf, by = c("sitename", "date") )
				  totlen <- length(datasource[ -which( datasource=="fluxnet2015" ) ])
				  ddf$gpp_obs <-  apply( dplyr::select( ddf, gpp_obs, gpp_obs_gepisat ), 1, stats::weighted.mean, c((totlen-1)/totlen, 1/totlen), na.rm=TRUE )

				} else {
				  ddf <- ddf %>% mutate( gpp_obs_gepisat = NA )
				}			  							
			}

			##------------------------------------------------------------
			## Filter days
			##------------------------------------------------------------
			if (!is.null(ettings_eval$filter_days)) ddf <- ddf %>% filter_days( settings_eval$filter_days, settings_eval$path_gepisat )

			##------------------------------------------------------------
			## Add forcing data to daily data frame (for neural network-based evaluation)
			##------------------------------------------------------------
			ddf <- lapply( as.list(settings_eval$sitenames), function(x) get_forcing_from_csv( x, settings_sims ) ) %>%
			       bind_rows() %>%
			       dplyr::select(-year_dec.x, -year_dec.y) %>%
						 right_join( ddf, by = c("sitename", "date") )

		  ##------------------------------------------------------------
		  ## Aggregate to multi-day periods
		  ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
		  ##------------------------------------------------------------
			# ## 8-day periods corresponding to MODIS dates (problem: doesn't start with Jan 1 each year)
	    	#  breaks <- modisdates <- readr::read_csv( "modisdates.csv" )$date

		  # ## aggregate to weeks
		  # xdf <- ddf %>% mutate( inbin = week(date) ) %>%
		  #                group_by( sitename, year, inbin ) %>%
		  #                summarise( gpp_obs = mean( gpp_obs, na.rm=TRUE) )

		  ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
			listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )	                 
			breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( settings_eval$agg, " days"), length.out = ceiling(365 / settings_eval$agg)) ) %>% Reduce(c,.)
		
			## take mean across periods
			xdf <- ddf %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
			 							 group_by( sitename, inbin ) %>%
			 							 summarise( gpp_obs_mean = mean( gpp_obs, na.rm = TRUE ), gpp_obs_min = min( gpp_obs, na.rm = TRUE ), gpp_obs_max = max( gpp_obs, na.rm = TRUE ), n_obs = sum(!is.na(gpp_obs)) ) %>%
			               dplyr::rename( gpp_obs = gpp_obs_mean ) %>%
			               mutate( gpp_obs = ifelse(is.nan(gpp_obs), NA, gpp_obs ), gpp_obs_min = ifelse(is.infinite(gpp_obs_min), NA, gpp_obs_min ), gpp_obs_max = ifelse(is.infinite(gpp_obs_max), NA, gpp_obs_max ) )
	}


	}

	return( list( ddf=ddf, xdf=xdf, mdf=mdf, adf=adf, breaks_xdf = breaks ) )
}
