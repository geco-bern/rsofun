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
#' (ddf for daily, xdf for X-daily, mdf for monthly, adf for annual), data frames of respective performance metrics,
#' and functions for plotting respective data.
#' @export
#'
#' @examples out_eval <- eval_sofun( mod, settings_eval, settings_sims, obs_eval = NA, overwrite = TRUE, doplot = FALSE )
#' 
eval_sofun <- function(mod, settings_eval, settings_sims, obs_eval = NA, overwrite = TRUE, doplot = FALSE){

  ## expand to flat data frame by rows
  mod <- mod$daily %>% bind_rows(.id = "sitename")
  
  ## Loop over variables to evaluate (benchmarking variables)
  out <- lapply( as.list(names(settings_eval$benchmark)), 
                 function(x) eval_sofun_byvar(x, dplyr::select(mod, sitename, date, mod = eval(x)), settings_eval, settings_sims, obs_eval = obs_eval, overwrite = TRUE, doplot = FALSE)
                 ) %>%
          setNames(names(settings_eval$benchmark))

  return(out)
}

eval_sofun_byvar <- function(varnam, ddf_mod, settings_eval, settings_sims, obs_eval = NA, overwrite = TRUE, doplot = FALSE){

  print("-----------------------------------")
  print(varnam)
  print("-----------------------------------")
  
  ## initialise
  out <- list()

  if ("fluxnet2015" %in% settings_eval$benchmark[[varnam]]){
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
    ## Initialise lists
    metrics <- list()

    ## get sites for which no model output is available and overwrite settings_eval$sitenames
    missing_mod <- purrr::map_lgl( mod, ~identical(., NA ) ) %>% which() %>% names()
    settings_eval$sitenames <- settings_eval$sitenames[which(!(settings_eval$sitenames %in% missing_mod))]
    
    ##------------------------------------------------------------
    ## Get daily model output
    ##------------------------------------------------------------
    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NA ) ) %>% which() %>% names()
    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NULL ) ) %>% which() %>% names()
    
    # ddf_mod <- lapply( 
    #   as.list(settings_eval$sitenames),  
    #   function(x) 
    #     dplyr::select( mod$daily[[x]], date, mod = eval(varnam) ) %>% 
    #     mutate( sitename = x ) ) %>%
    #   bind_rows()

    ##------------------------------------------------------------
    ## Get observations for evaluation
    ##------------------------------------------------------------
    if (identical(obs_eval, NA)) obs_eval <- get_obs_eval( settings_eval = settings_eval, settings_sims = settings_sims, overwrite = overwrite )
    
    ## detach
    if (varnam=="aet"){
      varnam_obs <- "latenth"
    } else {
      varnam_obs <- varnam
    }
    adf <- obs_eval$adf %>% select(sitename, date,  obs = eval(varnam_obs))
    mdf <- obs_eval$mdf %>% select(sitename, date,  obs = eval(varnam_obs))
    ddf <- obs_eval$ddf %>% select(sitename, date,  obs = eval(varnam_obs))
    xdf <- obs_eval$xdf %>% select(sitename, inbin, obs = eval(varnam_obs))
    
    obs_eval$adf <- NULL
    obs_eval$mdf <- NULL
    obs_eval$xdf <- NULL
    obs_eval$ddf <- NULL
    
    ##------------------------------------------------------------
    ## Aggregate model output data to annual/monthly/weekly, only for selected sites,
    ## and merge into respective observational data frame 
    ##------------------------------------------------------------
    print("Aggregating model outputs...")

    ## annual sum
    adf <- ddf_mod %>% 
      mutate( year = year(date) ) %>%
      group_by( sitename, year ) %>%
      summarise( mod = sum(mod), n = n() ) %>%
      mutate( mod = ifelse( n<365, NA, mod ) ) %>%
      ## merge into observational data frame
      right_join( mutate(adf, year = year(date)), by = c("sitename", "year")) %>%
      dplyr::filter( sitename %in% settings_eval$sitenames )

    ## monthly mean
    mdf <- ddf_mod %>% 
      mutate( year = year(date), moy = month(date) ) %>%
      group_by( sitename, year, moy ) %>%
      summarise( mod = mean(mod), n = n() ) %>%
      ## merge into observational data frame
      right_join( mutate( mdf, year = year(date), moy = month(date) ), by = c("sitename", "year", "moy")) %>%
      dplyr::filter( sitename %in% settings_eval$sitenames )

    ## mean across multi-day period
    xdf <- ddf_mod %>% 
      # mutate( year = year(date), week = week(date) ) %>%
      mutate( year = year(date), inbin = cut( date, breaks = obs_eval$breaks_xdf, right = FALSE ) ) %>%
      group_by( sitename, inbin ) %>%
      summarise( mod_mean = mean( mod, na.rm = TRUE ), mod_min = min( mod, na.rm = TRUE ), mod_max = max( mod, na.rm = TRUE ), n_mod = sum(!is.na(mod)) ) %>%
      dplyr::rename( mod = mod_mean ) %>%
      right_join( xdf, by = c("sitename", "inbin") ) %>%
      dplyr::filter( sitename %in% settings_eval$sitenames )
    
    ## daily
    ddf <- ddf_mod %>% 
      ## merge into observational data frame
      right_join( ddf, by = c("sitename", "date")) %>%
      dplyr::filter( sitename %in% settings_eval$sitenames )

    ## metrics for daily and x-daily values, all sites pooled
    metrics$daily_pooled  <- with( ddf, get_stats( mod, obs ) )
    metrics$xdaily_pooled <- with( xdf, get_stats( mod, obs ) )

    ##------------------------------------------------------------
    ## Evaluate annual values by site
    ##------------------------------------------------------------
    if (sum(!is.na(adf$obs))>2){
      print("Evaluate annual values...")
      adf_stats <- adf %>% 
                   mutate( validpoint = ifelse( is.na(obs) | is.na(mod), FALSE, TRUE ) ) %>% 
                   group_by( sitename ) %>% 
                   nest() %>%
                   mutate( npoints = purrr::map( data, ~sum( .$validpoint ) ) ) %>%
                   unnest( npoints ) %>%
                   filter( npoints > 2 ) %>%
                   mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                           stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
                   mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
                   unnest( stats )

      ## metrics for annual values, all sites pooled
      metrics$annual_pooled <- with( adf, get_stats( mod, obs ) )
    } else {
      adf_stats <- NA
      metrics$annual_pooled <- list( rsq=NA, rmse=NA )
    }

    ##------------------------------------------------------------
    ## Evaluate monthly values by site
    ##------------------------------------------------------------
    if (sum(!is.na(mdf$obs))>2){
      print("Evaluate monthly values...")
      mdf_stats <- mdf %>% group_by( sitename ) %>% 
                   nest() %>%
                   mutate( nmonths_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ),
                           nmonths_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
                   unnest( nmonths_obs, nmonths_mod ) %>%
                   filter( nmonths_obs > 2 & nmonths_mod > 0 ) %>%
                   mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = ., na.action = na.exclude ) ),
                           stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
                   mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
                   unnest( stats )

      ## metrics for annual values, all sites pooled
      metrics$monthly_pooled <- with( mdf, get_stats( mod, obs ) )
    } else {
      mdf_stats <- NA
      metrics$monthly_pooled <- list( rsq=NA, rmse=NA )
    }

    ##------------------------------------------------------------
    ## Get mean annual GPP -> "spatial" data frame and evaluate it
    ##------------------------------------------------------------
    if (sum(!is.na(adf$obs))>2){
      print("Evaluate spatial values...")
      meandf <- adf %>% group_by( sitename ) %>%
                summarise(  obs = mean( obs, na.rm=TRUE ),
                            mod = mean( mod, na.rm=TRUE ) )

      linmod_meandf <- lm( obs ~ mod, data = meandf ) 
      metrics$spatial <- with( meandf, get_stats( mod, obs ) )
      
      # ## test if identical data to previous evaluation
      # mymeandf <- meandf
      # load("tmpdir/../soilm_global/meandf_soilm_global.Rdata")
      # meandf <- meandf %>% dplyr::select( sitename=mysitename, obs_old=obs, mod_old=gpp_pmodel ) %>% 
      #           left_join( mymeandf, by="sitename" ) %>%
      #           mutate( diff = mod - mod_old )
      # with(meandf, plot( obs_old, obs))
      # lines(c(0,4000), c(0,4000))
      # with(meandf, plot( mod_old, mod))
      # lines(c(0,4000), c(0,4000))
      
      # ## daily values seem to be identical. something wrong with aggregating model outputs to annual values?
      # load( file="../soilm_global/data/nice_nn_agg_lue_obs_evi.Rdata" )
      # ddf_old <- nice_agg %>% dplyr::select( sitename = mysitename, date, mod_old = gpp_pmodel )
      # ddf_new <- lapply( as.list(settings_eval$sitenames),  function(x) dplyr::select( mod[[x]] , date, mod = gpp ) %>% mutate( sitename = x ) ) %>%
      #   bind_rows() %>% left_join( ddf_old, by=c("sitename", "date"))
      # with( filter(ddf_new, sitename=="AR-Vir"), plot(mod_old, mod) )
      # with( filter(ddf_new, sitename=="AU-ASM"), plot(mod_old, mod) )
      # with( filter(ddf_new, sitename=="US-Me2"), plot(mod_old, mod) )
      # with( ddf_new, plot( mod_old, mod ) )
    } else {
      meandf <- NA
      metrics$spatial <- list( rsq=NA, rmse=NA )
      linmod_meandf <- NA
    }
    
    ##------------------------------------------------------------
    ## Get IAV as annual value minus mean by site
    ##------------------------------------------------------------
    if (sum(!is.na(adf$obs))>2){
      print("Evaluate interannual variability...")
      iavdf <- adf %>% left_join( dplyr::rename( meandf, mod_mean = mod, obs_mean = obs ), by = "sitename" ) %>%
                mutate( mod = mod - mod_mean, 
                        obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean )
      
      iavdf_stats <- iavdf %>% 
                      group_by( sitename ) %>%
                      nest() %>%
                      mutate( nyears_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ), nyears_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
                      unnest( nyears_obs, nyears_mod ) %>%
                      filter( nyears_obs > 2 & nyears_mod > 2 ) %>%
                      mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                              stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
                      mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
                      unnest( stats )

      metrics$anomalies_annual <- with( iavdf, get_stats( mod, obs ) )
    } else {
      iavdf <- NA
      iavdf_stats <- NA
      metrics$anomalies_annual <- list( rsq=NA, rmse=NA )
    }
    
    ##------------------------------------------------------------
    ## Get mean seasonal cycle (by day of year)
    ##------------------------------------------------------------
    if (sum(!is.na(ddf$obs))>2){
      print("Evaluate mean seasonal cycle...")
      meandoydf <- ddf %>%  mutate( doy = yday(date) ) %>%
                    filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
                    group_by( sitename, doy ) %>% 
                    summarise( obs_mean = mean( obs, na.rm=TRUE ), obs_min = min( obs, na.rm=TRUE ), obs_max = max( obs, na.rm=TRUE ),
                               mod_mean = mean( mod, na.rm=TRUE ), mod_min = min( mod, na.rm=TRUE ), mod_max = max( mod, na.rm=TRUE )
                               ) %>%
                    mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
                    mutate( obs_mean_plot = interpol_lin(obs_mean), obs_min_plot = interpol_lin(obs_min), obs_max_plot = interpol_lin( obs_max ), site=sitename )

      meandoydf_stats <- meandoydf %>% group_by( sitename ) %>%
                         nest()

      metrics$meandoy <- with( meandoydf, get_stats( mod_mean, obs_mean ) )

      ## aggregate mean seasonal cycle by climate zone (koeppen-geiger) and hemisphere (pooling sites within the same climate zone)
      print("Evaluate mean seasonal cycle by climate zones...")
      meandoydf_byclim <- ddf %>% mutate( doy = yday(date) ) %>%
                          left_join( dplyr::select( metainfo_Tier1_sites_kgclimate_fluxnet2015, sitename, lat, koeppen_code ), by = "sitename" ) %>%   # 'metainfo_Tier1_sites_kgclimate_fluxnet2015' is lazy-loaded with library(rsofun)
                          mutate( hemisphere = ifelse( lat>0, "north", "south" ) ) %>%
                          # mutate( bias = mod - obs ) %>% 
                          dplyr::select( -lat ) %>%
                          filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
                          group_by( koeppen_code, hemisphere, doy ) %>% 
                          summarise( obs_mean  = median( obs, na.rm=TRUE ), obs_min  = quantile( obs, 0.33, na.rm=TRUE ), obs_max  = quantile( obs, 0.66, na.rm=TRUE ),
                                     mod_mean  = median( mod, na.rm=TRUE ), mod_min  = quantile( mod, 0.33, na.rm=TRUE ), mod_max  = quantile( mod, 0.66, na.rm=TRUE ),
                                     # bias_mean = median( bias,    na.rm=TRUE ), bias_min = quantile( bias, 0.33,    na.rm=TRUE ), bias_max = quantile( bias,    0.66, na.rm=TRUE ),
                                     nsites = length(unique(sitename)) ) %>%
                          mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
                          mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ) ) %>%
                          mutate( climatezone = paste( koeppen_code, hemisphere ) )
      
      meandoydf_byclim_stats <- meandoydf_byclim %>% 
                                group_by( koeppen_code, hemisphere ) %>%
                                nest() %>%
                                mutate( n_obs  = purrr::map( data, ~sum(!is.na(.$obs_mean)) ) ) %>% 
                                unnest( n_obs ) %>% 
                                filter( n_obs>0, !is.na(koeppen_code) ) %>% 
                                mutate( linmod = purrr::map( data, ~lm( obs_mean ~ mod_mean, data = . ) ),
                                        stats  = purrr::map( data, ~get_stats( .$mod_mean, .$obs_mean ) ) ) %>%
                                mutate( data   = purrr::map( data, ~add_fitted_alternativenames(.) ) ) %>%
                                unnest( stats )
      
      metrics$meandoy_byclim <- meandoydf_byclim_stats %>% dplyr::select( -data, -linmod )
      
    } else {
      
      meandoydf <- NA 
      meandoydf_stats <- NA 
      metrics$meandoy <- list( rsq=NA, rmse=NA )
      meandoydf_byclim <- NA 
      meandoydf_byclim_stats <- NA 
      metrics$meandoy_byclim <- list( rsq=NA, rmse=NA )
    
    }


    ##------------------------------------------------------------
    ## Get IDV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
    if (sum(!is.na(ddf$obs))>2){
      print("Evaluate inter-day variability...")
      idvdf <- ddf %>%  mutate( doy = yday(date) ) %>%
                left_join( dplyr::rename( meandoydf, mod_mean = mod_mean, obs_mean = obs_mean ), by = c("sitename", "doy") ) %>%
                mutate( mod = mod - mod_mean, obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
      
      idvdf_stats <- idvdf %>% 
                      group_by( sitename ) %>%
                      nest() %>%
                      mutate( ndays_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ), ndays_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
                      unnest( ndays_obs, ndays_mod ) %>%
                      filter( ndays_obs > 2 & ndays_mod > 2 ) %>%
                      mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                              stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
                      mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
                      unnest( stats )                   
      
      metrics$anomalies_daily <- with( idvdf, get_stats( mod, obs ) )
      
    } else {
      
      idvdf <- NA
      idvdf_stats <- NA
      metrics$anomalies_daily <- list( rsq=NA, rmse=NA )
    
    }
    
    ##------------------------------------------------------------
    ## Get mean seasonal cycle (by week (or X-day period) of year)
    ##------------------------------------------------------------
    if (sum(!is.na(xdf$obs))>2){
      print("Evaluate mean seasonal cycle by X-day periods...")
      meanxoydf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
                    group_by( sitename, xoy ) %>% 
                    summarise( obs_mean = mean( obs, na.rm=TRUE ), obs_min = min( obs, na.rm=TRUE ), obs_max = max( obs, na.rm=TRUE ),
                               mod_mean = mean( mod, na.rm=TRUE ), mod_min = min( mod, na.rm=TRUE ), mod_max = max( mod, na.rm=TRUE )
                               ) %>%
                    mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
                    mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

      meanxoydf_stats <- meanxoydf %>% group_by( sitename ) %>%
                         nest()

      metrics$meanxoy <- with( meanxoydf, get_stats( mod_mean, obs_mean ) )
      
    } else {
      
      meanxoydf <- NA
      meanxoydf_stats <- NA
      metrics$meanxoy <- list( rsq=NA, rmse=NA )
      
    }

    ##------------------------------------------------------------
    ## Get IXV (inter-day variability) as daily value minus mean by site and DOY
    ##------------------------------------------------------------
    if (sum(!is.na(xdf$obs))>2){
      print("Evaluate inter-X-day variability...")
      ixvdf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
                left_join( dplyr::rename( meanxoydf, mod_mean = mod_mean, obs_mean = obs_mean ), by = c("sitename", "xoy") ) %>%
                mutate( mod = mod - mod_mean, obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean, -obs_min, -obs_max) #-mod_min, -mod_max
      
      ixvdf_stats <- ixvdf %>% 
                      group_by( sitename ) %>%
                      nest() %>%
                      mutate( nxdays_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ), nxdays_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
                      unnest( nxdays_obs, nxdays_mod ) %>%
                      filter( nxdays_obs > 2 & nxdays_mod > 2 ) %>%
                      mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                              stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
                      mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
                      unnest( stats )
      
      metrics$anomalies_xdaily <- with( ixvdf, get_stats( mod, obs ) )
      
    } else {
      
      ixvdf <- NA
      ixvdf_stats <- NA
      metrics$anomalies_xdaily <- list( rsq=NA, rmse=NA )
      
    }

    print("Done with eval_sofun().")


    ##------------------------------------------------------------
    ## FLUXNET2015-Plots
    ##------------------------------------------------------------

      ##------------------------------------------------------------
      ## Mod. vs. obs. of mean per site -> spatial correlation
      ##------------------------------------------------------------
      plot_modobs_spatial <- function( makepdf=FALSE ){   # using meandf
        
        # source("analyse_modobs.R")
        par(las=1, mar=c(4,4.5,4,1))
        if (!dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs ) )
        if (makepdf) { filn <- paste0( settings_eval$dir_figs, "/modobs_spatial.pdf" ) } else { filn <- NA }
        if (nrow(meandf)>2){
          modobs_spatial <- with( meandf, analyse_modobs( 
            mod, 
            obs, 
            heat = FALSE, 
            col = "black", 
            ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
            plot.fil = filn,
            plot.title = "Spatial correlation"
          ) )
        } else {
          modobs_spatial <- NA
          if (makepdf) pdf(filn)
          with( meandf, plot( 
            mod, 
            obs,  
            col = "black",
            pch = 16,
            ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
            main = "Spatial correlation",
            xlim = range(c(mod, obs, na.rm=TRUE)),
            ylim = range(c(mod, obs, na.rm=TRUE))
          ))
          lines( c(-9999,9999), c(-9999,9999), lty=3 )
          if (makepdf) dev.off()
        }
        # abline( linmod_meandf, col="red")
        return(modobs_spatial)
      }

      ##------------------------------------------------------------
      ## Combined spatial - IAV correlation
      ##------------------------------------------------------------
      plot_modobs_spatial_annual <- function( pattern = "", makepdf = FALSE, ... ){  #meandf, linmod_meandf, annual_bysite_stats, annual_pooled_stats = NA, spatial_stats = NA, 

        if (!identical(linmod_meandf, NA)){
          
          if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
          if (makepdf) filn <- paste0( settings_eval$dir_figs, "/modobs_spatial_annual_", pattern, ".pdf" )
          if (makepdf) print( paste( "Plotting to file:", filn ) )
          if (makepdf) pdf( filn )
          
          par(las=1, mar=c(4,4.5,4,1))
          
          ## set up plotting and add linear regression line for means by site
          with( meandf, plot( mod, obs, pch=16, col=rgb(0,0,0,0.5), type = "n", ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ), ... ) )
          abline( linmod_meandf, col="red")
          lines(c(-9999,9999), c(-9999,9999), lty=3)
          
          ## plot black regression lines of annual values within sites
          out <- adf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ mod, data = . ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., mod ) ) )
          
          title( "Spatial/annual correlation" )

          ## Add annotations for statistics of annual values (pooled)
          if (!identical(NA, metrics$annual_pooled)) mtext( bquote( italic(R)^2 == .(format( metrics$annual_pooled$rsq,  digits = 2) ) ), adj = 1, cex = 0.8, line=2 )
          if (!identical(NA, metrics$annual_pooled)) mtext( paste0( "RMSE = ",       format( metrics$annual_pooled$rmse, digits = 3 ) ),  adj = 1, cex = 0.8, line=1 )
          
          ## Add annotations for statistics of means by site (~spatial)
          if (!identical(NA, metrics$spatial)) mtext( bquote( italic(R)^2 == .(format( metrics$spatial$rsq, digits = 2) ) ), adj = 0, cex = 0.8, line=2, col="red" )
          if (!identical(NA, metrics$spatial)) mtext( paste0( "RMSE = ",  format( metrics$spatial$rmse, digits = 3 ) ), adj = 0, cex = 0.8, line=1, col="red" )
          # if (!identical(NA, metrics$spatial)) mtext( paste0( "slope = ", format( metrics$spatial$meanslope, digits = 3 ) ), adj = 0, cex = 0.8, col="red" )
          
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
          # if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
          # if (makepdf) pdf( paste0( settings_eval$dir_figs, "/hist_slopes_anomalies_annual.pdf" ) )
          #   hist( annual_bysite_stats$slope, xlim=c(-5,5), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 50, xlab="slope" )
          #   abline( v=1.0, col="red" )
          #   axis( 1, cex.axis=1.0, xlab="slope" )
          #   title( "Slopes of annual regressions" )
          # if (makepdf) dev.off()
          # 
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
          # if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
          # if (makepdf) pdf( paste0( settings_eval$dir_figs, "/hist_r2_anomalies_annual.pdf" ) )
          #   hist( annual_bysite_stats$rsq, xlim=c(-1,1), cex.axis=0.7, axes=FALSE, col="grey70", main="", breaks = 12, xlab= bquote( italic(R)^2 ) )
          #   abline( v=1.0, col="red" )
          #   axis( 1, cex.axis=1.0, xlab = bquote( italic(R)^2 ) )
          #   title( bquote( bold(Slopes ~ of ~ italic(R)^2) ) )
          # if (makepdf) dev.off()
          
        } else {
          print("plot_modobs_spatial_annual(): Not enough sites to get spatial correlations.")
        }
        

      }

      ##------------------------------------------------------------
      ## Mod. vs. obs. of IAV correlation: x_(y,i) - mean_y( x_(y,i) )
      ##------------------------------------------------------------
      plot_modobs_anomalies_annual <- function( makepdf = FALSE ){   # using iavdf, iavdf_stats
        # source("analyse_modobs.R")
        if(makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_anomalies_annual.pdf") )
          par(las=1)
          modobs_anomalies_annual <- with( iavdf, analyse_modobs(mod, 
            obs, 
            heat = FALSE,
            ylab = expression( paste("observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "yr"^-1, ")" ) ),
            plot.title = "IAV correlation"
           ))
          out <- iavdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ mod, data = ., col=rgb(0,0,1,0.3) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., mod ) ) )
        if(makepdf) dev.off()
        return(modobs_anomalies_annual)
      }


      ##------------------------------------------------------------
      ## Mod. vs. obs. of IDV (interday variability) correlation: x_(d,i) - mean_d( x_(d,i) )
      ##------------------------------------------------------------
      plot_modobs_anomalies_daily <- function( pattern="", makepdf = FALSE ){   # using idvdf, idvdf_stats
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_anomalies_daily_", pattern, ".pdf" ) )
          modobs_anomalies_daily <- with( idvdf, analyse_modobs(
            mod,
            obs,
            col=rgb(0,0,0,0.05),
            ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ),
            xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )
            ))
          out <- idvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ mod, data = ., col=rgb(0,0,1,0.05) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., mod ) ) )
          title( "IDV correlation" )
        if (makepdf) dev.off()
        
        ## histogram of daily anomalies from mean seasonal cycle based on DOY
        ##------------------------------------------------------------
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/hist_anomalies_daily_", pattern, ".pdf" ) )
          par(las=1)
          out <- with( idvdf, hist( obs, breaks = 50, col = rgb(0,0,0,0.3), freq = FALSE, main = "Daily anomalies", ylim = c(0,0.6), xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ) ) )
          with( idvdf, hist( mod, breaks = metrics$breaks, col = rgb(1,0,0,0.3), freq = FALSE, add = TRUE ) )
          mtext( bquote( sigma[obs] == .(format( sd(idvdf$obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 ) 
          mtext( bquote( sigma[mod] == .(format( sd(idvdf$mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )  
          legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
        if (makepdf) dev.off()

        return(modobs_anomalies_daily)

      }   


      ##------------------------------------------------------------
      ## Mod. vs. obs. of m of IXV correlation: x_(x,i) - mean_x( x_(x,i) )
      ##------------------------------------------------------------
      plot_modobs_anomalies_xdaily <- function( makepdf = FALSE ){  # using ixvdf, ixvdf_stats
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_anomalies_xdaily.pdf" ) )
          modobs_anomalies_xdaily <- with( ixvdf, analyse_modobs(
            mod, 
            obs, 
            col=rgb(0,0,0,0.05), 
            ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) )
            ))
          out <- ixvdf_stats %>%  mutate( purrr::map( data, ~lines( fitted ~ mod, data = ., col=rgb(0,0,1,0.1) ) ) )  # to have it sorted: %>% mutate( data = purrr::map( data, ~arrange( ., mod ) ) )
          title( "IXV correlation" )
        if (makepdf) dev.off()

        ## histogram of X-daily anomalies from mean seasonal cycle based on XOY
        ##------------------------------------------------------------
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/hist_anomalies_xdaily.pdf" ) )
          par(las=1)
          ## do not plot, to get density
          outhist1 <- with( ixvdf, hist( obs, breaks = 20, plot = FALSE ) )
          outhist2 <- with( ixvdf, hist( mod, breaks = outhist1$breaks, freq = FALSE, plot = FALSE ) )

          ## plot with proper y-axis
          plot(outhist1, freq = FALSE, col = rgb(0,0,0,0.3), main = "Anomalies in X-day periods", xlab = expression( paste("GPP anomaly (gC m"^-2, "d"^-1, ")" ) ), ylim = c(0,max(outhist1$density, outhist2$density)))
          plot(outhist2, freq = FALSE, add=TRUE, col = rgb(1,0,0,0.3))

          mtext( bquote( sigma[obs] == .(format( sd(ixvdf$obs, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=0 ) 
          mtext( bquote( sigma[mod] == .(format( sd(ixvdf$mod, na.rm = TRUE), digits = 3)) ), side=3, adj=0, line=-1 )  
          legend("topright", c("observed", "modelled"), fill = c(rgb(0,0,0,0.3), rgb(1,0,0,0.3)), bty = "n")
        if (makepdf) dev.off()
        return(modobs_anomalies_xdaily)
      }  


      ##------------------------------------------------------------
      ## Mod. vs. obs. of mean seasonal cycle by day of year (DOY)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_meandoy <- function( pattern = "", makepdf = FALSE ){    # using meandoydf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_meandoy_", pattern, ".pdf") )
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


      ##------------------------------------------------------------
      ## Wrapper for mean seasonality by site (daily) for selected sites only sites
      ##------------------------------------------------------------
      plot_by_doy_allsites <- function( makepdf = FALSE ){   # using meandoydf_stats
        system( "mkdir -p fig/meandoy_bysite" )
        # mylist <- readr::read_csv("myselect_fluxnet2015.csv") %>% filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
        mylist <- c("AU-Tum", "CA-NS3", "CA-NS6", "CA-Obs", "DE-Geb", "DE-Hai", "DE-Kli", "FI-Hyy", "FR-Fon", "FR-LBr", "FR-Pue", "IT-Cpz", "NL-Loo", "US-Ha1", "US-MMS", "US-UMB", "US-WCr")
        tmp <- purrr::map( filter( meandoydf_stats, sitename %in% mylist )$data, ~plot_by_doy_bysite(., makepdf = makepdf) )
      }


      ##------------------------------------------------------------
      ## Wrapper for mean seasonality by site (daily) for all climate zones
      ##------------------------------------------------------------
      plot_by_doy_allzones <- function( dashed = NA, makepdf = FALSE, pattern="" ){    # using meandoydf_byclim_stats 
        system( "mkdir -p fig/meandoy_byzone" )
        # tmp <- purrr::map( meandoydf_byclim_stats$data, ~plot_by_doy_byzone(., dashed = dashed, makepdf = makepdf, pattern = pattern ) )
        tmp <- purrr::map( as.list(seq(nrow(meandoydf_byclim_stats))) , ~plot_by_doy_byzone( meandoydf_byclim_stats$data[[.]], makepdf = makepdf, pattern = pattern ) )  # dashed = dashed$data[[.]]
      }


      ##------------------------------------------------------------
      ## Mean seasonal cycle by x-day-period of year (XOY)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_meanxoy <- function( makepdf = FALSE ){    # meanxoydf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_meanxoy.pdf" ) )
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


      ##------------------------------------------------------------
      ## Wrapper for mean seasonality by site for all sites (aggregated by X-day periods)
      ##------------------------------------------------------------
      plot_by_xoy_allsites <- function( makepdf = FALSE ){     # using meanxoydf_stats
        system( "mkdir -p fig/meanxoy_bysite" )
        tmp <- purrr::map( meanxoydf_stats$data, ~plot_by_xoy_bysite(., makepdf = makepdf ) )
      }


      ##------------------------------------------------------------
      ## Mod. vs. obs for daily values (absolute)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_daily <- function( subtitle = "", makepdf = FALSE, ... ){  # ddf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_daily.pdf" ) )
        modobs_ddf <- with( ddf, 
          analyse_modobs( 
            mod, 
            obs, 
            heat=TRUE, 
            ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
            plot.title = "Daily GPP",
            plot.subtitle = subtitle,
            ...
          ) )
        if (makepdf) dev.off()
        return( modobs_ddf )
      }

      ##------------------------------------------------------------
      ## Mod. vs. obs for monthly values (absolute)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_monthly <- function( makepdf = FALSE, ... ){  # using mdf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_monthly.pdf" ) )
        modobs_mdf <- with( mdf, 
          analyse_modobs( 
            mod, 
            obs, 
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
      ## Mod. vs. obs for nnual values (absolute)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_annual <- function( makepdf = FALSE, ... ){  # using adf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_annual.pdf" ) )
        modobs_adf <- with( adf, 
          analyse_modobs( 
            mod, 
            obs, 
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
      ## Mod. vs. obs. for ggregated values (absolute) aggregated to X-day periods
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_xdaily <- function( makepdf = FALSE, ... ){    # using xdf
        # source("analyse_modobs.R")
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/modobs_xdaily.pdf" ) )
        modobs_xdf <- with( xdf, 
          analyse_modobs( 
            mod, 
            obs, 
            heat=TRUE, 
            ylab = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
            xlab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
            plot.title = "Correlation of mean GPP in X-day periods",
            ...
            ) )
        if (makepdf) dev.off()
        return(modobs_xdf)
      }


      ##------------------------------------------------------------
      ## Mean seasonality (daily) for one site
      ##------------------------------------------------------------
      plot_by_doy_bysite <- function( df, makepdf = FALSE ){
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/meandoy_bysite/meandoy_bysite_", df$site[1], ".pdf" ))
          par(las=1)
          yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
          plot(  df$doy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
          polygon( c(df$doy, rev(df$doy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
          lines( df$doy, df$mod_mean, col="red", lwd=1.75 )
          polygon( c(df$doy, rev(df$doy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
          title( paste0( "Mean seasonality (daily) - ", df$site[1] ) )
        if (makepdf) dev.off()
      }

      ##------------------------------------------------------------
      ## Mean seasonality with aggregated data from one climate zone 
      ##------------------------------------------------------------
      plot_by_doy_byzone <- function( df, dashed = NA, makepdf = FALSE, pattern="" ){
        if (df$nsites[1]>4){
          dir_figs <- paste0( settings_eval$dir_figs, "/meandoy_byzone" )
          if (makepdf && !dir.exists(dir_figs)) system( paste0( "mkdir -p ", dir_figs))
          if (makepdf) filn <- paste0( dir_figs, "/meandoy_byzone_", df$climatezone[1], "_", pattern, ".pdf" )
          if (makepdf) print( paste( "Plotting to file:", filn ) )
          if (makepdf) pdf( filn )
            par(las=1)
            yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
            plot(  df$doy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
            polygon( c(df$doy, rev(df$doy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
            lines( df$doy, df$mod_mean, col="red", lwd=1.75 )
            if (!is.na(dashed)) lines( dashed$doy, dashed$mod_mean, col="red", lwd=0.75, lty=1 )
            polygon( c(df$doy, rev(df$doy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
            title( df$climatezone[1] )
            mtext( bquote( italic(N) == .( df$nsites[1])), side=3, line=1, cex=1.0, adj=1.0 )
          if (makepdf) dev.off()
        } else {
          rlang::warn( paste0("plot_by_doy_byzone(): Number of sites below 5 for climate zone ", df$climatezone[1]) )
        }
      }

      ##------------------------------------------------------------
      ## Mean seasonality (X-day periods) for one site
      ##------------------------------------------------------------
      plot_by_xoy_bysite <- function( df, makepdf = FALSE ){
        if (makepdf && !dir.exists(settings_eval$dir_figs)) system( paste0( "mkdir -p ", settings_eval$dir_figs))
        if (makepdf) pdf( paste0( settings_eval$dir_figs, "/meanxoy_bysite/meanxoy_bysite_", df$site[1], ".pdf" ))
          par(las=1)
          yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
          plot(  df$xoy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
          polygon( c(df$xoy, rev(df$xoy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
          lines( df$xoy, df$mod_mean, col="red", lwd=1.75 )
          polygon( c(df$xoy, rev(df$xoy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
          title( paste0( "Mean seasonality (X-daily) - ", df$site[1] ) )
        if (makepdf) dev.off()
      }  


    ##------------------------------------------------------------
    ## Construct output lists for FLUXNET2015
    ##------------------------------------------------------------
    out$fluxnet2015 <- list()

    out$fluxnet2015$data <- list(  
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
      adf                    = adf,
      mdf                    = mdf,
      ddf                    = ddf, 
      xdf                    = xdf
      )

    out$fluxnet2015$plot <- list(
      modobs_daily            = plot_modobs_daily,
      modobs_xdaily           = plot_modobs_xdaily,
      modobs_monthly          = plot_modobs_monthly,
      modobs_annual           = plot_modobs_annual,
      modobs_spatial          = plot_modobs_spatial,
      modobs_spatial_annual   = plot_modobs_spatial_annual,
      modobs_anomalies_annual = plot_modobs_anomalies_annual,
      modobs_anomalies_daily  = plot_modobs_anomalies_daily,
      modobs_anomalies_xdaily = plot_modobs_anomalies_xdaily,
      modobs_meandoy          = plot_modobs_meandoy,
      by_doy_allsites         = plot_by_doy_allsites,
      by_doy_allzones         = plot_by_doy_allzones,
      modobs_meanxoy          = plot_modobs_meanxoy,
      by_xoy_allsites         = plot_by_xoy_allsites
      )

    out$fluxnet2015$metrics <- metrics

  } # end FLUXNET2015

  return(out)

}

add_fitted <- function( data ){
  linmod <- lm( obs ~ mod, data = data, na.action = "na.exclude" )
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

extract_koeppen_code <- function( str ){
  out <- stringr::str_split( str, " - ")[[1]][1]
  return( out )
}
