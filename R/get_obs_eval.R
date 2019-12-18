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
#' @examples obs <- get_obs_eval( settings_eval, settings_sims, overwrite = TRUE )
#' 
get_obs_eval <- function( settings_eval, settings_sims, overwrite = TRUE, light = FALSE, add_forcing = FALSE){

  ## Interpret benchmarking data specification
  datasource <- settings_eval$benchmark %>% 
    unlist() %>% 
    stringr::str_split( ., "_" ) %>% 
    unlist()
  
  ## determine variables (original names in files) to be read from benchmark data
  evalvars <- names(settings_eval$benchmark)
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
    getvars <- c(getvars, "LE_F_MDS", "LE_F_MDS_QC", "LE_RANDUNC")
    evalvars[which(evalvars=="aet")] <- "latenth"
    settings_eval$benchmarkvar$latenth <- "LE_F_MDS"
  }
    
  if ("fluxnet2015" %in% datasource){
    ##------------------------------------------------------------
    ## Read annual observational data from FLUXNET 2015 files (from annual files!).
    ##------------------------------------------------------------
    ## loop over sites to get data frame with all variables
    if (settings_eval$path_fluxnet2015_y!="" && !("Ty" %in% datasource)){
      print("getting annual FLUXNET-2015_Tier1 data...")
      print(settings_eval$path_fluxnet2015_y)
      adf <- lapply( as.list(settings_eval$sitenames),
                     function(x) get_obs_bysite_fluxnet2015(
                       sitename = x,
                       path_fluxnet2015 = settings_eval$path_fluxnet2015_y,
                       path_fluxnet2015_hh = NULL,
                       timescale = "y",
                       getvars = getvars,
                       getswc=!light,
                       threshold_GPP=0,
                       remove_neg = TRUE,
                       verbose=FALSE
                       ) %>%
                       mutate( year =lubridate::year(date),
                               sitename = x )) %>%
        bind_rows()

      ## change variable names
      if ("gpp" %in% names(settings_eval$benchmark)){
        adf <- adf %>%
          change_names("gpp", settings_eval$benchmarkvar)
      }


    } else {

      adf <-  lapply( as.list(settings_eval$sitenames),
                      function(x) init_dates_dataframe(
                        year(settings_sims$date_start[[x]]),
                        year(settings_sims$date_end[[x]]),
                        noleap = TRUE,
                        freq = "years"
                      ) %>%
                        ## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
                        mutate( year =lubridate::year(date),
                                sitename = x )) %>%
        bind_rows()

    }

    ## remove pre-modis data
    if (settings_eval$remove_premodis){
      adf <- adf %>% filter( lubridate::year(date) >= 2000 )
    }

    ##------------------------------------------------------------
    ## Read monthly observational data from FLUXNET 2015 files (from monthly files!).
    ##------------------------------------------------------------
    ## loop over sites to get data frame with all variables
    if (settings_eval$path_fluxnet2015_m!="" && !("Ty" %in% datasource)){
      print("getting monthly FLUXNET-2015_Tier1 data...")
      mdf <- lapply( as.list(settings_eval$sitenames),
                     function(x) get_obs_bysite_fluxnet2015(
                       sitename = x,
                       path_fluxnet2015 = settings_eval$path_fluxnet2015_m,
                       path_fluxnet2015_hh = NULL,
                       timescale = "m",
                       getvars = getvars,
                       getswc=!light,
                       threshold_GPP=0.5,
                       remove_neg = TRUE,
                       verbose=FALSE
                       ) %>%
                       mutate( year =lubridate::year(date),
                               sitename = x )) %>%
        bind_rows()

      ## change variable names
      if ("gpp" %in% names(settings_eval$benchmark)){
        mdf <- mdf %>%
          change_names("gpp", settings_eval$benchmarkvar)
      }

    } else {

      mdf <-  lapply( as.list(settings_eval$sitenames),
                      function(x) init_dates_dataframe(
                        year(settings_sims$date_start[[x]]),
                        year(settings_sims$date_end[[x]]),
                        noleap = TRUE,
                        freq = "months"
                      ) %>%
                        ## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
                        mutate( year =lubridate::year(date),
                                sitename = x )) %>%
        bind_rows()
    }

    ## remove pre-modis data
    if (settings_eval$remove_premodis){
      mdf <- mdf %>% filter(lubridate::year(date) >= 2000 )
    }

    ##------------------------------------------------------------
    ## Read daily observational data from FLUXNET 2015 files (from daily files!).
    ##------------------------------------------------------------
    if (settings_eval$path_fluxnet2015_d!="" && !("Ty" %in% datasource)){
      print("getting daily FLUXNET-2015_Tier1 data...")
      ddf <- lapply( as.list(settings_eval$sitenames),
                     function(x) get_obs_bysite_fluxnet2015( 
                       sitename = x, 
                       path_fluxnet2015 = settings_eval$path_fluxnet2015_d, 
                       path_fluxnet2015_hh = NULL,
                       timescale = "d", 
                       getvars = getvars, 
                       getswc=!light,                              
                       threshold_GPP=0.5, 
                       remove_neg = FALSE,
                       verbose=TRUE
                       ) %>%
                       mutate( year =lubridate::year(date),
                               sitename = x )) %>% 
        bind_rows()

        ## change variable names
        if ("gpp" %in% names(settings_eval$benchmark)){
          ddf <- ddf %>% 
            change_names("gpp", settings_eval$benchmarkvar)
        }
      
    } else {
      
      ddf <-  lapply( as.list(settings_eval$sitenames),
                      function(x) init_dates_dataframe(
                        year(settings_sims$date_start[[x]]),
                        year(settings_sims$date_end[[x]]),
                        noleap = TRUE,
                        freq = "days" 
                      ) %>%
                        ## Remove outliers, i.e. when data is outside 1.5 times the inter-quartile range
                        mutate( year =lubridate::year(date),
                                sitename = x )) %>%
        bind_rows()
      
    }
    
    ## remove pre-modis data
    if (settings_eval$remove_premodis){
      ddf <- ddf %>% filter(lubridate::year(date) >= 2000 )
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
      ddf_gepisat <- ddf_gepisat %>% 
        bind_rows( .id = "sitename" ) %>%
        mutate( gpp_obs = ifelse( date < "2000-02-18", NA, gpp_obs ) ) %>%  # remove pre-modis data
        dplyr::rename( gpp = gpp_obs ) %>% 
        
        # remove weird data points where error is zero
        mutate( gpp = ifelse( gpp_err_obs==0.0, NA, gpp ) )
      
      
      ## ake weighted average for updated 'obs', aggregate, and add to other data frames (ddf, adf, mdf)
      if (!is.null(ddf_gepisat)){
        
        ## daily
        ddf <- ddf_gepisat %>% 
          right_join( ddf, by = c("sitename", "date") ) %>% 
          dplyr::select(-year, -year_dec)
        
        ## don't remember why I did this, commenting it out
        # totlen <- length(datasource[ -which( datasource=="fluxnet2015" ) ])
        #ddf$gpp <-  apply( dplyr::select( ddf, gpp, gpp_gepisat ), 1, stats::weighted.mean, c((totlen-1)/totlen, 1/totlen), na.rm=TRUE )
        
        # ##------------------------------------------------------------
        # ## Add forcing data to daily data frame (for evaluation of functional relationships)
        # ##------------------------------------------------------------
        # ddf <- lapply( as.list(settings_eval$sitenames), function(x) get_forcing_from_csv( x, settings_sims ) ) %>%
        #   bind_rows() %>%
        #   dplyr::select(-year_dec.x, -year_dec.y) %>%
        #   right_join( ddf, by = c("sitename", "date") )
        
        ## monthly
        mdf <- mdf %>% mutate( year=year(date), moy=month(date) )
        mdf <- ddf %>%
          mutate( moy=month(date), year=year(date) ) %>% 
          group_by( sitename, year, moy ) %>% 
          summarise( gpp = sum(gpp) ) %>% 
          right_join( mdf, by = c("sitename", "year", "moy") )
        
        ## annual
        adf <- ddf %>%
          mutate( year=year(date) ) %>% 
          group_by( sitename, year ) %>% 
          summarise( gpp = sum(gpp) ) %>% 
          right_join( adf, by = c("sitename", "year") )
        
      } else {
        ddf <- ddf %>% mutate( gpp_gepisat = NA )
      }
    }
    
    ##------------------------------------------------------------
    ## Add forcing data to daily data frame (for evaluation of functional relationships)
    ##------------------------------------------------------------
    if (add_forcing){
      print("adding forcing data...")
      ddf <- lapply( as.list(settings_eval$sitenames), function(x) get_forcing_from_csv( x, settings_sims ) ) %>%
        bind_rows() %>%
        right_join( ddf, by = c("sitename", "date") )
    }

    ##------------------------------------------------------------
    ## Aggregate to multi-day periods
    ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
    ##------------------------------------------------------------
    # ## 8-day periods corresponding to MODIS dates (problem: doesn't start with Jan 1 each year)
    #  breaks <- modisdates <- readr::read_csv( "modisdates.csv" )$date
    
    # ## aggregate to weeks
    # xdf <- ddf %>% mutate( inbin = week(date) ) %>%
    #                group_by( sitename, year, inbin ) %>%
    #                summarise( gpp = mean( gpp, na.rm=TRUE) )
    
    ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
    listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )    
    breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( settings_eval$agg, " days"), length.out = ceiling(365 / settings_eval$agg)) ) %>% Reduce(c,.)

    ## take mean across periods
    xdf <- ddf %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
      group_by( sitename, inbin ) %>%
      summarise_at( vars(one_of(evalvars)), mean, na.rm=TRUE)

  }

  return( list( ddf = ddf, xdf = xdf, mdf = mdf, adf = adf, breaks_xdf = breaks ) )
}


## Read forcing data from CSV file prepared for SOFUN input
get_forcing_from_csv <- function( sitename, settings_sims ){
  
  ## get climate data
  dir <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename )
  csvfiln <- paste0( dir, "/clim_daily_lev2_", sitename, ".csv" )
  if (file.exists(csvfiln)){
    ddf <- readr::read_csv( csvfiln )
  } else {
    ddf <- tibble(date=ymd("2001-01-01"))
  }
  
  ## get fapar data
  dir <- paste0( settings_sims$path_input, "/sitedata/fapar/", sitename )
  csvfiln <- paste0( dir, "/fapar_daily_", sitename, ".csv" )
  if (file.exists(csvfiln)){
    ddf <- readr::read_csv( csvfiln ) %>%
      mutate( fapar = as.numeric(modisvar_interpol)) %>%
      dplyr::select(date, fapar) %>% 
      right_join( ddf, by = "date" )
  } else {
    ddf <- tibble(date=ymd("2001-01-01"))
  }
  
  return(ddf)
  
}

change_names <- function(df, varnam, benchmarkvar){
  
  df[[varnam]] <- df[[benchmarkvar[[varnam]]]]
  df <- df %>% 
    dplyr::select(-matches(benchmarkvar[[varnam]]))
  
  if (varnam=="gpp"){
    df <- df %>% 
      dplyr::select(-starts_with("GPP_"))
  }
  
  return(df)      
}
