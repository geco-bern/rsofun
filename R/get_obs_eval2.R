#' Get observational data for model evaluation
#'
#' Gets observational data for model evaluation, given the benchmarking variable, and data source used 
#' for the evaluation. This information is specified in the evaluation settings (argument \code{settings_eval}).
#'
#' @param settings_eval A list specifying evaluation settings (see vignette eval_sofun.pdf for more information and examples)
#' @param settings_sims A named list containing the simulation settings (see vignette_rsofun.pdf for more information and examples)
#' @param overwrite (Optional) A logical specifying whether temporary data stored in \code{./tmpdir} should be overwritten. Defaults to \code{TRUE}.
#' @param light (Optional) A logical specifying whether reduced data should saved. Defaults to \code{FALSE}.
#' @param add_forcing (Optional) a logical specifying whether forcing data is to be added to data frames containing observational data.
#'
#' @return A list containing data frames of observed values aggregated to several temporal scales (ddf for daily, xdf for X-daily, mdf for monthly, adf for annual).
#' @export
#'
#' @examples obs <- get_obs_eval2( siteinfo = NULL, settings_eval = NULL, adf = NULL, mdf = NULL, ddf = NULL, hrdf = NULL, hhdf = NULL, agg = 8 )
#' 
get_obs_eval2 <- function( siteinfo = NULL, settings_eval = NULL, adf = NULL, mdf = NULL, ddf = NULL, hrdf = NULL, hhdf = NULL, agg = 8 ){

  evalvars <- names(settings_eval$benchmark)
  
	if (is.null(adf) || is.null(mdf) || is.null(ddf)){  # nothing done with hourly or half-hourly data yet
		##------------------------------------------
	  ## Interpret benchmarking data specification
		##------------------------------------------
	  datasource <- settings_eval$benchmark %>% 
	    unlist() %>% 
	    stringr::str_split( ., "_" ) %>% 
	    unlist()
	  
	  if ("fluxnet2015" %in% datasource){    
	    
	    if (is.null(adf) || is.null(mdf) || is.null(ddf)){
	      ##------------------------------------------
	      ## determine variables (original names in files) to be read from benchmark data
	      ##------------------------------------------
	      getvars <- c()
	      if ("gpp" %in% names(settings_eval$benchmark)){
	        ## Get observational GPP data
	        getvars <- c(getvars, "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC")
	        settings_eval$benchmarkvar$gpp <- ifelse(settings_eval$benchmark$gpp == "fluxnet2015_NT",
	                                                 "GPP_NT_VUT_REF",
	                                                 ifelse(settings_eval$benchmark$gpp == "fluxnet2015_DT",
	                                                        "GPP_DT_VUT_REF",
	                                                        NA))
	      }
	      if ("netrad" %in% names(settings_eval$benchmark)){
	        getvars <- c(getvars, "NETRAD")
	        settings_eval$benchmarkvar$netrad <- "NETRAD"
	      }
	      if ("aet" %in% names(settings_eval$benchmark) || "latenth" %in% names(settings_eval$benchmark)){
	        getvars <- list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC", latenth_unc = "LE_RANDUNC")
	        getvars <- c(getvars, "LE_F_MDS", "LE_F_MDS_QC", "LE_RANDUNC")
	        settings_eval$benchmarkvar$latenth <- "LE_F_MDS"
	      }
	    }

			##------------------------------------------
			## Ingest data
			##------------------------------------------
			if (is.null(adf)){
				adf_eval <- ingest(
				  siteinfo = siteinfo,
				  source    = "fluxnet2015", 
				  getvars   = getvars,
				  dir       = settings_eval$path_fluxnet2015_y,
				  settings  = list(threshold_LE = 0.8, getswc = TRUE),
				  timescale = "y"
				  )
			}

			if (is.null(ddf)){
				ddf_eval <- ingest(
				  siteinfo = siteinfo,
				  source    = "fluxnet2015", 
				  getvars   = getvars,
				  dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
				  settings  = list(threshold_LE = 0.8, getswc = TRUE),
				  timescale = "d"
				  )
			}

			if (is.null(mdf)){	
				mdf_eval <- ingest(
				  siteinfo = siteinfo,
				  source    = "fluxnet2015", 
				  getvars   = getvars,
				  dir       = settings_eval$path_fluxnet2015_m,
				  settings  = list(threshold_LE = 0.8, getswc = TRUE),
				  timescale = "m"
				  )
			}
		}
	}


  ##------------------------------------------------------------
  ## Aggregate daily data to multi-day periods
  ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
  ##------------------------------------------------------------
  ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
  listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )    
  breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( agg, " days"), length.out = ceiling(365 / agg)) ) %>% Reduce(c,.)

  ## take mean across periods
  xdf <- ddf %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
    group_by( sitename, inbin ) %>%
    summarise_at( vars(one_of(evalvars)), mean, na.rm=TRUE)

	return( list( ddf = ddf, xdf = xdf, mdf = mdf, adf = adf, breaks_xdf = breaks ) )

}
