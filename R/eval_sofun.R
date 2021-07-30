#' Evaluates SOFUN model outputs.
#'
#' Calculates a set of perfomance metrics for model outputs, compared against observational data.
#' Currently only evaluations of GPP model outputs, compared agains FLUXNET 2015 data, are implemented.
#'
#' @param mod Object returned by \link{runread_pmodel_f}. This is a nested dataframe with sites along rows 
#' and a nested column \code{"data"} containing model outputs with columns \code{"date"} (date object 
#' created by \code{lubridate::ymd()}) and \code{"varnam"}.
#' where \code{"varnam"} corresponds to \code{names(settings$benchmark)}.
#' @param settings A list specifying evaluation settings 
#' (see vignette eval_sofun.pdf for more information and examples)
#' @param obs_eval (Optional) A named list of data frames containing observational data for each sites. 
#' The names of list elements corresponds to site names. Defaults to \code{NA} 
#' @param overwrite (Optional) A logical specifying whether temporary data stored in \code{./tmpdir} should be overwritten. Defaults to \code{TRUE}.
#' @param doplot (Optional) A logical specifying whether plots should be saved. Defaults to \code{FALSE}.
#' @param light (Optional) A logical specifying whether reduced data should saved. Defaults to \code{FALSE}.
#'
#' @return A list containing data frames of modelled and observed values aggregated to several temporal scales 
#' (ddf for daily, xdf for X-daily, mdf for monthly, adf for annual), data frames of respective performance metrics,
#' and functions for plotting respective data.
#' @export
#'
#' @examples out_eval <- eval_sofun( mod, settings, obs_eval = NA, overwrite = TRUE, doplot = FALSE )
#' 
eval_sofun <- function(mod, settings, obs_eval = NA, overwrite = TRUE, doplot = FALSE, light = FALSE){
  
  ## make model output a long flat table
  mod <- mod %>% 
    # dplyr::rename(id = sitename) %>% 
    tidyr::unnest(data)
  
  ## Evaluate daily variables
  out <- purrr::map(
    as.list(names(settings$benchmark)),
    ~eval_sofun_byvar(., dplyr::select(mod, sitename, date, mod = {{.}}), settings, obs_eval = obs_eval, overwrite = TRUE, doplot = FALSE, light = light)
    ) %>% 
    setNames(names(settings$benchmark))
  
  # out <- lapply( as.list(names(settings$benchmark)), 
  #                function(x) eval_sofun_byvar(x, dplyr::select(mod, sitename, date, mod = eval(x)), settings, obs_eval = obs_eval, overwrite = TRUE, doplot = FALSE, light = light)
  #                ) %>%
  #         setNames(names(settings$benchmark))

  return(out)
}

eval_sofun_byvar <- function(varnam, ddf_mod, settings, obs_eval = NA, overwrite = TRUE, doplot = FALSE, light = light){

  rlang::inform("-----------------------------------")
  rlang::inform(varnam)
  rlang::inform("-----------------------------------")
  
  ## initialise
  out <- list()
  
  ## Interpret benchmarking data specification
  datasource <- settings$benchmark[[varnam]] %>% 
    stringr::str_split( ., "_" ) %>% 
    unlist()

  if ("fluxnet" %in% datasource){
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

    ## get sites for which no model output is available and overwrite settings$sitenames
    missing_mod <- purrr::map_lgl( ddf_mod, ~identical(., NA ) ) %>% which() %>% names()
    settings$sitenames <- settings$sitenames[which(!(settings$sitenames %in% missing_mod))]
    
    ##------------------------------------------------------------
    ## Get daily model output
    ##------------------------------------------------------------
    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NA ) ) %>% which() %>% names()
    # missing_mod <- purrr::map_lgl( mod$daily, ~identical(., NULL ) ) %>% which() %>% names()
    
    # ddf_mod <- lapply( 
    #   as.list(settings$sitenames),  
    #   function(x) 
    #     dplyr::select( mod$daily[[x]], date, mod = eval(varnam) ) %>% 
    #     mutate( sitename = x ) ) %>%
    #   bind_rows()

    ##------------------------------------------------------------
    ## Get observations for evaluation
    ##------------------------------------------------------------
    if (identical(obs_eval, NA)) rlang::abort("eval_sofun_byvar(): Object provided by argument 'eval_sofun_byvar' could not be identified.")
    
    ## detach
    if (varnam=="aet"){
      varnam_obs <- "latenth"
    } else {
      varnam_obs <- varnam
    }
    adf <- obs_eval$adf %>% tidyr::unnest(data) %>% dplyr::select(sitename, date,  obs = {{varnam_obs}})
    mdf <- obs_eval$mdf %>% tidyr::unnest(data) %>% dplyr::select(sitename, date,  obs = {{varnam_obs}})
    ddf <- obs_eval$ddf %>% tidyr::unnest(data) %>% dplyr::select(sitename, date,  obs = {{varnam_obs}}, lat, koeppen_code)
    xdf <- obs_eval$xdf %>% tidyr::unnest(data) %>% dplyr::select(sitename, inbin, obs = {{varnam_obs}})
    
    # obs_eval$adf <- NULL
    # obs_eval$mdf <- NULL
    # obs_eval$xdf <- NULL
    # obs_eval$ddf <- NULL
    
    ##------------------------------------------------------------
    ## Aggregate model output data to annual/monthly/weekly, only for dplyr::selected sites,
    ## and merge into respective observational data frame 
    ##------------------------------------------------------------
    rlang::inform("Aggregating model outputs...")

    ## annual sum
    adf <- ddf_mod %>% 
      tidyr::drop_na() %>% 
      mutate( year = year(date) ) %>%
      group_by( year, sitename ) %>%
      summarise( mod = sum(mod), n = n() ) %>%
      mutate( mod = ifelse( n<365, NA, mod ) ) %>%
      ## merge into observational data frame
      right_join( mutate(adf, year = year(date)), by = c("sitename", "year")) 
      # dplyr::filter( sitename %in% settings$sitenames )

    ## monthly mean
    mdf <- mdf %>% ungroup()
    mdf <- ddf_mod %>% 
      mutate( year = year(date), moy = month(date) ) %>%
      group_by( sitename, year, moy ) %>%
      summarise( mod = mean(mod), n = n() ) %>%
      ## merge into observational data frame
      right_join( mutate( mdf, year = year(date), moy = month(date) ), by = c("sitename", "year", "moy"))
      # dplyr::filter( sitename %in% settings$sitenames )

    ## mean across multi-day period
    xdf <- ddf_mod %>% 
      # mutate( year = year(date), week = week(date) ) %>%
      mutate( year = year(date), inbin = cut( date, breaks = obs_eval$breaks, right = FALSE ) ) %>%
      tidyr::drop_na() %>% 
      group_by( sitename, inbin ) %>%
      summarise( mod_mean = mean( mod, na.rm = TRUE ), mod_min = min( mod, na.rm = TRUE ), mod_max = max( mod, na.rm = TRUE ), n_mod = sum(!is.na(mod)) ) %>%
      dplyr::rename( mod = mod_mean ) %>%
      right_join( xdf, by = c("sitename", "inbin") )
      # dplyr::filter( sitename %in% settings$sitenames )
    
    ## daily
    ddf <- ddf_mod %>% 
      tidyr::drop_na() %>% 
      ## merge into observational data frame
      right_join( ddf, by = c("sitename", "date"))
      # dplyr::filter( sitename %in% settings$sitenames )

    ## metrics for daily and x-daily values, all sites pooled
    metrics$daily_pooled  <- with( ddf, get_stats( mod, obs ) )
    metrics$xdaily_pooled <- with( xdf, get_stats( mod, obs ) )

    ##------------------------------------------------------------
    ## Evaluate annual values by site
    ##------------------------------------------------------------
    if (sum(!is.na(adf$obs))>2){
      rlang::inform("Evaluate annual values...")
      if (!light){
        adf_stats <- adf %>% 
          mutate( validpoint = ifelse( is.na(obs) | is.na(mod), FALSE, TRUE ) ) %>% 
          group_by( sitename ) %>% 
          nest() %>%
          mutate( npoints = purrr::map( data, ~sum( .$validpoint ) ) ) %>%
          unnest( npoints ) %>%
          dplyr::filter( npoints > 2 ) %>%
          mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                  stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
          mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
          unnest( stats )
      } else {
        adf_stats <- NA
      }

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
      rlang::inform("Evaluate monthly values...")
      ## xxx is not used
      # mdf_stats <- mdf %>% group_by( sitename ) %>% 
      #              nest() %>%
      #              mutate( nmonths_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ),
      #                      nmonths_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
      #              unnest( nmonths_obs, nmonths_mod ) %>%
      #              dplyr::filter( nmonths_obs > 2 & nmonths_mod > 0 ) %>%
      #              mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = ., na.action = na.exclude ) ),
      #                      stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
      #              mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
      #              unnest( stats )
      mdf_stats <- NA
      
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
      rlang::inform("Evaluate spatial values...")
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
      # ddf_new <- lapply( as.list(settings$sitenames),  function(x) dplyr::select( mod[[x]] , date, mod = gpp ) %>% mutate( sitename = x ) ) %>%
      #   bind_rows() %>% left_join( ddf_old, by=c("sitename", "date"))
      # with( dplyr::filter(ddf_new, sitename=="AR-Vir"), plot(mod_old, mod) )
      # with( dplyr::filter(ddf_new, sitename=="AU-ASM"), plot(mod_old, mod) )
      # with( dplyr::filter(ddf_new, sitename=="US-Me2"), plot(mod_old, mod) )
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
      rlang::inform("Evaluate interannual variability...")
      iavdf <- adf %>% left_join( dplyr::rename( meandf, mod_mean = mod, obs_mean = obs ), by = "sitename" ) %>%
                mutate( mod = mod - mod_mean, 
                        obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean )
      
      if (!light){
        iavdf_stats <- iavdf %>% 
          group_by( sitename ) %>%
          nest() %>%
          mutate( nyears_obs = purrr::map( data, ~sum(!is.na( .$obs )  ) ), nyears_mod = purrr::map( data, ~sum(!is.na( .$mod )  ) ) ) %>%
          unnest( nyears_obs, nyears_mod ) %>%
          dplyr::filter( nyears_obs > 2 & nyears_mod > 2 ) %>%
          mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                  stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
          mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
          unnest( stats )
      } else {
        iavdf_stats <- NA
      }

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
      rlang::inform("Evaluate mean seasonal cycle...")
      meandoydf <- ddf %>%  mutate( doy = yday(date) ) %>%
                    dplyr::filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
                    group_by( sitename, doy ) %>% 
                    summarise( obs_mean = mean( obs, na.rm=TRUE ), obs_min = min( obs, na.rm=TRUE ), obs_max = max( obs, na.rm=TRUE ),
                               mod_mean = mean( mod, na.rm=TRUE ), mod_min = min( mod, na.rm=TRUE ), mod_max = max( mod, na.rm=TRUE )
                               ) %>%
                    mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
                    mutate( obs_mean_plot = interpol_lin(obs_mean), obs_min_plot = interpol_lin(obs_min), obs_max_plot = interpol_lin( obs_max ), site=sitename )

      if (!light){
        meandoydf_stats <- meandoydf %>% group_by( sitename ) %>%
          nest()
      } else {
        meandoydf_stats <- NA
      }

      metrics$meandoy <- with( meandoydf, get_stats( mod_mean, obs_mean ) )

      ## aggregate mean seasonal cycle by climate zone (koeppen-geiger) and hemisphere (pooling sites within the same climate zone)
      if (!light){
        rlang::inform("Evaluate mean seasonal cycle by climate zones...")
        meandoydf_byclim <- ddf %>% mutate( doy = yday(date) ) %>%
          # left_join( dplyr::select( metainfo_Tier1_sites_kgclimate_fluxnet2015, sitename, lat, koeppen_code ), by = "sitename" ) %>%   # 'metainfo_Tier1_sites_kgclimate_fluxnet2015' is lazy-loaded with library(rsofun)
          mutate( hemisphere = ifelse( lat>0, "north", "south" ) ) %>%
          # mutate( bias = mod - obs ) %>% 
          dplyr::select( -lat ) %>%
          dplyr::filter( doy != 366 ) %>% ## XXXX this is a dirty fix! better force lubridate to ignore leap years when calculating yday()
          group_by( koeppen_code, hemisphere, doy ) %>% 
          summarise( obs_mean  = median( obs, na.rm=TRUE ), obs_min  = quantile( obs, 0.33, na.rm=TRUE ), obs_max  = quantile( obs, 0.66, na.rm=TRUE ),
                     mod_mean  = median( mod, na.rm=TRUE ), mod_min  = quantile( mod, 0.33, na.rm=TRUE ), mod_max  = quantile( mod, 0.66, na.rm=TRUE ),
                     # bias_mean = median( bias,    na.rm=TRUE ), bias_min = quantile( bias, 0.33,    na.rm=TRUE ), bias_max = quantile( bias,    0.66, na.rm=TRUE ),
                     nsites = length(unique(sitename)) ) %>%
          mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
          mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ) ) %>%
          mutate( climatezone = paste( koeppen_code, hemisphere ) )
        
        # meandoydf_byclim_stats <- meandoydf_byclim %>% 
        #   group_by( koeppen_code, hemisphere ) %>%
        #   nest() %>%
        #   mutate( n_obs  = purrr::map( data, ~sum(!is.na(.$obs_mean)) ) ) %>% 
        #   unnest( n_obs ) %>% 
        #   dplyr::filter( n_obs>0, !is.na(koeppen_code) ) %>% 
        #   mutate( linmod = purrr::map( data, ~lm( obs_mean ~ mod_mean, data = . ) ),
        #           stats  = purrr::map( data, ~get_stats( .$mod_mean, .$obs_mean ) ) ) %>%
        #   mutate( data   = purrr::map( data, ~add_fitted_alternativenames(.) ) ) %>%
        #   unnest( stats )
        # 
        # metrics$meandoy_byclim <- meandoydf_byclim_stats %>% dplyr::select( -data, -linmod )
        meandoydf_byclim_stats <- NA
        
      } else {
        
        meandoydf_byclim_stats <- NA
        metrics$meandoy_byclim <- list( rsq=NA, rmse=NA )
        meandoydf_byclim <- NA
        
      }
      
      
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
      rlang::inform("Evaluate inter-day variability...")
      idvdf <- ddf %>%  mutate( doy = yday(date) ) %>%
                left_join( dplyr::rename( meandoydf, mod_mean = mod_mean, obs_mean = obs_mean ), by = c("sitename", "doy") ) %>%
                mutate( mod = mod - mod_mean, obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean, -obs_min, -obs_max, -mod_min, -mod_max )
      
      if (!light){
        use_sites <- idvdf %>% 
          group_by(sitename) %>% 
          summarise(ndays_obs = sum(!is.na(obs)), ndays_mod = sum(!is.na(mod)), var_mod = var(mod, na.rm = TRUE)) %>% 
          dplyr::filter(ndays_obs > 0 & ndays_mod > 0 & var_mod > 0) %>% 
          pull(sitename)
        
        idvdf_stats <- idvdf %>% 
          dplyr::filter(sitename %in% use_sites) %>% 
          group_by( sitename ) %>%
          nest() %>%
          mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                  stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
          mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
          unnest( stats )    
        
        idvdf_stats <- NA
      } else {
        idvdf_stats <- NA
      }
      
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
      rlang::inform("Evaluate mean seasonal cycle by X-day periods...")
      meanxoydf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
                    group_by( sitename, xoy ) %>% 
                    summarise( obs_mean = mean( obs, na.rm=TRUE ), obs_min = min( obs, na.rm=TRUE ), obs_max = max( obs, na.rm=TRUE ),
                               mod_mean = mean( mod, na.rm=TRUE ), mod_min = min( mod, na.rm=TRUE ), mod_max = max( mod, na.rm=TRUE )
                               ) %>%
                    mutate( obs_min = ifelse( is.infinite(obs_min), NA, obs_min ), obs_max = ifelse( is.infinite(obs_max), NA, obs_max ) ) %>%
                    mutate( obs_mean = interpol_lin(obs_mean), obs_min = interpol_lin(obs_min), obs_max = interpol_lin( obs_max ), site=sitename )

      if (!light){
        meanxoydf_stats <- meanxoydf %>% group_by( sitename ) %>%
          nest()
      } else {
        meanxoydf_stats <- NA
      }

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
      rlang::inform("Evaluate inter-X-day variability...")
      ixvdf <- xdf %>%  mutate( xoy = yday(inbin) ) %>%
                left_join( dplyr::rename( meanxoydf, mod_mean = mod_mean, obs_mean = obs_mean ), by = c("sitename", "xoy") ) %>%
                mutate( mod = mod - mod_mean, obs = obs - obs_mean ) %>%
                dplyr::select( -obs_mean, -mod_mean, -obs_min, -obs_max) #-mod_min, -mod_max
      
      if (!light){
        use_sites <- idvdf %>% 
          group_by(sitename) %>% 
          summarise(ndays_obs = sum(!is.na(obs)), ndays_mod = sum(!is.na(mod)), var_mod = var(mod, na.rm = TRUE)) %>% 
          dplyr::filter(ndays_obs > 0 & ndays_mod > 0 & var_mod > 0) %>% 
          pull(sitename)
        
        ixvdf_stats <- ixvdf %>% 
          dplyr::filter(sitename %in% use_sites) %>% 
          group_by( sitename ) %>%
          nest() %>%
          mutate( linmod = purrr::map( data, ~lm( obs ~ mod, data = . ) ),
                  stats  = purrr::map( data, ~get_stats( .$mod, .$obs ) ) ) %>%
          mutate( data   = purrr::map( data, ~add_fitted(.) ) ) %>%
          unnest( stats )
        
      } else {
        
        ixvdf_stats <- NA
      
      }
      
      metrics$anomalies_xdaily <- with( ixvdf, get_stats( mod, obs ) )
      
      
    } else {
      
      ixvdf <- NA
      ixvdf_stats <- NA
      metrics$anomalies_xdaily <- list( rsq=NA, rmse=NA )
      
    }

    ##------------------------------------------------------------
    ## FLUXNET2015-Plots
    ##------------------------------------------------------------
    if (!light){
      ##------------------------------------------------------------
      ## Mod. vs. obs. of mean per site -> spatial correlation
      ##------------------------------------------------------------
      plot_modobs_spatial <- function( makepdf=FALSE ){   # using meandf
        
        if (!dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs ) )
        if (makepdf) { filn <- paste0( settings$dir_figs, "/modobs_spatial.pdf" ) } else { filn <- NA }
        
        if (nrow(meandf)>2){
          
          out <- meandf %>% 
            rbeni::analyse_modobs2("mod", "obs", type = "points") + 
            labs(title = "Spatial correlation")
          
        } else {
          modobs_spatial <- NA
          
          gg <- meandf %>%
            ggplot2::ggplot(aes(mod, obs)) +
            geom_point() +
            geom_abline(intercept=0, slope=1, linetype="dotted") +
            labs(title = "Spatial correlation")
          
          out <- list(gg=gg, df_metrics=NA)
        }
        
        if (makepdf) ggsave(filn)
        
        return(out)
      }
      
      ##------------------------------------------------------------
      ## Combined spatial - IAV correlation
      ##------------------------------------------------------------
      plot_modobs_spatial_annual <- function(){

        get_start_end <- function(df){
          df_start <- df %>% 
            arrange(mod) %>% 
            drop_na(mod, fitted) %>% 
            slice(1)
          df_end <- df %>% 
            arrange(desc(mod)) %>% 
            drop_na(mod, fitted) %>% 
            slice(1)
          out <- tibble(
            xmin = df_start$mod, 
            xmax = df_end$mod,
            ymin = df_start$fitted,
            ymax = df_end$fitted )
          return(out)
        }
        df <- adf_stats %>% 
          mutate(start_end = purrr::map(data, ~get_start_end(.))) %>% 
          tidyr::unnest(start_end)
        
        rsq_lab_annual <-  format(metrics$annual_pooled$rsq, digits = 2)
        rmse_lab_annual <- format(metrics$annual_pooled$rmse, digits = 3)
        
        rsq_lab_spatial <-  format(metrics$spatial$rsq, digits = 2)
        rmse_lab_spatial <- format(metrics$spatial$rmse, digits = 3)
        
        gg <- df %>% 
          ggplot() +
          geom_segment(aes(x=xmin, y=ymin, xend=xmax, yend=ymax)) +
          geom_line(data = fortify(linmod_meandf), aes(x = mod, y = .fitted), color="red") +
          geom_abline(intercept=0, slope=1, linetype="dotted") +
          # ggrepel::geom_text_repel(data = df, aes(x = xmax, y = ymax, label = mylabel)) +
          theme_classic() +
          xlim(0,NA) +
          ylim(0,NA) +
          labs(
            subtitle = bquote( bold("Annual:") ~ italic(R)^2 == .(rsq_lab_annual) ~~
                                 RMSE == .(rmse_lab_annual) ~ "\n" ~
                                 bold("Spatial:") ~ italic(R)^2 == .(rsq_lab_spatial) ~~
                                 RMSE == .(rmse_lab_spatial) ),
            y = expression( paste("Observed GPP (gC m"^-2, "yr"^-1, ")" ) ), 
            x = expression( paste("Simulated GPP (gC m"^-2, "yr"^-1, ")" ) ))
        
        return(gg)
      }
      
      ##------------------------------------------------------------
      ## Mod. vs. obs. of IAV correlation: x_(y,i) - mean_y( x_(y,i) )
      ##------------------------------------------------------------
      plot_modobs_anomalies_annual <- function( makepdf = FALSE ){   # using iavdf, iavdf_stats
        # source("analyse_modobs.R")
        if(makepdf) pdf( paste0( settings$dir_figs, "/modobs_anomalies_annual.pdf") )
        par(las=1)
        modobs_anomalies_annual <- with( iavdf, rbeni::analyse_modobs2(mod, 
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/modobs_anomalies_daily_", pattern, ".pdf" ) )
        modobs_anomalies_daily <- with( idvdf, rbeni::analyse_modobs2(
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/hist_anomalies_daily_", pattern, ".pdf" ) )
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/modobs_anomalies_xdaily.pdf" ) )
        modobs_anomalies_xdaily <- with( ixvdf, rbeni::analyse_modobs2(
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/hist_anomalies_xdaily.pdf" ) )
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/modobs_meandoy_", pattern, ".pdf") )
        modobs_meandoy <- with( meandoydf, 
                                rbeni::analyse_modobs2( 
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
        # mylist <- readr::read_csv("myselect_fluxnet.csv") %>% dplyr::filter( use==1 ) %>% dplyr::select( -use ) %>% unlist()
        mylist <- c("AU-Tum", "CA-NS3", "CA-NS6", "CA-Obs", "DE-Geb", "DE-Hai", "DE-Kli", "FI-Hyy", "FR-Fon", "FR-LBr", "FR-Pue", "IT-Cpz", "NL-Loo", "US-Ha1", "US-MMS", "US-UMB", "US-WCr")
        tmp <- purrr::map( dplyr::filter( meandoydf_stats, sitename %in% mylist )$data, ~plot_by_doy_bysite(., makepdf = makepdf) )
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/modobs_meanxoy.pdf" ) )
        modobs_meanxoy <- with( meanxoydf, 
                                rbeni::analyse_modobs2( 
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
      plot_modobs_daily <- function(){
        
        modobs_ddf <- ddf %>% 
          rbeni::analyse_modobs2(mod = "mod", obs = "obs", type = "density")
        
        gg <- modobs_ddf$gg +
          labs(x = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
               y = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
               title = "Daily GPP")
        
        return( gg )
      }
      
      ##------------------------------------------------------------
      ## Mod. vs. obs for monthly values (absolute)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_monthly <- function(){
        
        modobs_mdf <- mdf %>% 
          rbeni::analyse_modobs2(mod = "mod", obs = "obs", type = "heat")
        
        gg <- modobs_mdf$gg +
          labs(x = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
               y = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
               title = "Monthly GPP")
        
        return( gg )
      }
      
      
      ##------------------------------------------------------------
      ## Mod. vs. obs for nnual values (absolute)
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_annual <- function(){
        modobs_adf <- adf %>% 
          rbeni::analyse_modobs2(mod = "mod", obs = "obs", type = "points")
        
        gg <- modobs_adf$gg +
          labs(x = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
               y = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
               title = "Annual GPP")
        
        return( gg )
      }
      
      
      ##------------------------------------------------------------
      ## Mod. vs. obs. for ggregated values (absolute) aggregated to X-day periods
      ##------------------------------------------------------------
      ## observed vs. modelled
      plot_modobs_xdaily <- function(){
        modobs_ddf <- xdf %>% 
          rbeni::analyse_modobs2(mod = "mod", obs = "obs", type = "heat")
        
        gg <- modobs_ddf$gg +
          labs(x = expression( paste("observed GPP (gC m"^-2, "d"^-1, ")" ) ), 
               y = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ),
               title = "X-daily GPP")
        return(gg)
      }
      
      
      ##------------------------------------------------------------
      ## Mean seasonality (daily) for one site
      ##------------------------------------------------------------
      plot_by_doy_bysite <- function( df, makepdf = FALSE ){
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/meandoy_bysite/meandoy_bysite_", df$site[1], ".pdf" ))
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
          dir_figs <- paste0( settings$dir_figs, "/meandoy_byzone" )
          if (makepdf && !dir.exists(dir_figs)) system( paste0( "mkdir -p ", dir_figs))
          if (makepdf) filn <- paste0( dir_figs, "/meandoy_byzone_", df$climatezone[1], "_", pattern, ".pdf" )
          if (makepdf) rlang::inform( paste( "Plotting to file:", filn ) )
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
        if (makepdf && !dir.exists(settings$dir_figs)) system( paste0( "mkdir -p ", settings$dir_figs))
        if (makepdf) pdf( paste0( settings$dir_figs, "/meanxoy_bysite/meanxoy_bysite_", df$site[1], ".pdf" ))
        par(las=1)
        yrange <- range( df$mod_min, df$mod_max, df$obs_min, df$obs_max, na.rm = TRUE )
        plot(  df$xoy, df$obs_mean, type="l", ylim = yrange, ylab = expression( paste("simulated GPP (gC m"^-2, "d"^-1, ")" ) ), xlab = "DOY" )
        polygon( c(df$xoy, rev(df$xoy)), c(df$obs_min, rev(df$obs_max)), border = NA, col = rgb(0,0,0,0.3)  )
        lines( df$xoy, df$mod_mean, col="red", lwd=1.75 )
        polygon( c(df$xoy, rev(df$xoy)), c(df$mod_min, rev(df$mod_max)), border = NA, col = rgb(1,0,0,0.3)  )
        title( paste0( "Mean seasonality (X-daily) - ", df$site[1] ) )
        if (makepdf) dev.off()
      }  

    }

    rlang::inform("Done with eval_sofun().")
    
    ##------------------------------------------------------------
    ## Construct output lists for FLUXNET2015
    ##------------------------------------------------------------
    out$fluxnet <- list()

    out$fluxnet$data <- list(  
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

    out$fluxnet$metrics <- metrics
    
    if (!light){
      
      out$fluxnet$plot <- list(
        gg_modobs_daily            = plot_modobs_daily(),
        gg_modobs_xdaily           = plot_modobs_xdaily(),
        gg_modobs_monthly          = plot_modobs_monthly(),
        gg_modobs_annual           = plot_modobs_annual(),
        modobs_spatial             = plot_modobs_spatial,
        gg_modobs_spatial_annual   = plot_modobs_spatial_annual(),
        modobs_anomalies_annual = plot_modobs_anomalies_annual,
        modobs_anomalies_daily  = plot_modobs_anomalies_daily,
        modobs_anomalies_xdaily = plot_modobs_anomalies_xdaily,
        modobs_meandoy          = plot_modobs_meandoy,
        by_doy_allsites         = plot_by_doy_allsites,
        by_doy_allzones         = plot_by_doy_allzones,
        modobs_meanxoy          = plot_modobs_meanxoy,
        by_xoy_allsites         = plot_by_xoy_allsites
      )
    }
    
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

## copied from rbeni package
analyse_modobs2 <- function(
  df,
  mod,
  obs,
  type       = "points",
  filnam     = NA,
  relative   = FALSE,
  xlim       = NULL,
  ylim       = NULL,
  use_factor = NULL,
  shortsubtitle = FALSE,
  plot_subtitle = TRUE,
  plot_linmod = TRUE,
  ...
  ){

  require(ggplot2)
  require(dplyr)
  require(LSD)
  require(ggthemes)
  require(RColorBrewer)

  #if (identical(filnam, NA)) filnam <- "analyse_modobs.pdf"

  ## rename to 'mod' and 'obs' and remove rows with NA in mod or obs
  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(mod=mod, obs=obs) %>%
    tidyr::drop_na(mod, obs)

  ## get linear regression (coefficients)
  linmod <- lm( obs ~ mod, data=df )

  ## construct metrics table using the 'yardstick' library
  df_metrics <- df %>%
    yardstick::metrics(obs, mod) %>%
    dplyr::bind_rows( tibble( .metric = "n",        .estimator = "standard", .estimate = summarise(df, numb=n()) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "slope",    .estimator = "standard", .estimate = coef(linmod)[2]) ) %>%
    # dplyr::bind_rows( tibble( .metric = "nse",      .estimator = "standard", .estimate = hydroGOF::NSE( obs, mod, na.rm=TRUE ) ) ) %>%
    dplyr::bind_rows( tibble( .metric = "mean_obs", .estimator = "standard", .estimate = summarise(df, mean=mean(obs, na.rm=TRUE)) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "prmse",    .estimator = "standard",
                       .estimate = dplyr::filter(., .metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() /
                         dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pmae",    .estimator = "standard",
                       .estimate = dplyr::filter(., .metric=="mae") %>% dplyr::select(.estimate) %>% unlist() /
                         dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "bias",        .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod-obs), na.rm=TRUE    )) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pbias",       .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod-obs)/obs, na.rm=TRUE)) %>% unlist() ) )

  rsq_val <- df_metrics %>% dplyr::filter(.metric=="rsq") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  rmse_val <- df_metrics %>% dplyr::filter(.metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  mae_val <- df_metrics %>% dplyr::filter(.metric=="mae") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  bias_val <- df_metrics %>% dplyr::filter(.metric=="bias") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  slope_val <- df_metrics %>% dplyr::filter(.metric=="slope") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  n_val <- df_metrics %>% dplyr::filter(.metric=="n") %>% dplyr::select(.estimate) %>% unlist() %>% unname()

  if (relative){
    rmse_val <- rmse_val / mean(df$obs, na.rm = TRUE)
    bias_val <- bias_val / mean(df$obs, na.rm = TRUE)
  }

  rsq_lab <- format( rsq_val, digits = 2 )
  rmse_lab <- format( rmse_val, digits = 3 )
  mae_lab <- format( mae_val, digits = 3 )
  bias_lab <- format( bias_val, digits = 3 )
  slope_lab <- format( slope_val, digits = 3 )
  n_lab <- format( n_val, digits = 3 )

  results <- tibble( rsq = rsq_val, rmse = rmse_val, mae = mae_val, bias = bias_val, slope = slope_val, n = n_val )

  if (shortsubtitle){
    subtitle <- bquote( italic(R)^2 == .(rsq_lab) ~~
                          RMSE == .(rmse_lab) )
  } else {
    subtitle <- bquote( italic(R)^2 == .(rsq_lab) ~~
                          RMSE == .(rmse_lab) ~~
                          bias == .(bias_lab) ~~
                          slope == .(slope_lab) ~~
                          italic(N) == .(n_lab) )
  }

  if (type=="heat"){

    # if (!identical(filnam, NA)) dev.off()
    # source("~/LSD/R/LSD.heatscatter.R")

    gg <- heatscatter(
                  df$mod,
                  df$obs,
                  xlim=xlim,
                  ylim=ylim,
                  main="",
                  ggplot=TRUE )

    gg <- gg +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)
    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="hex"){

    ## ggplot hexbin
    gg <- df %>%
      ggplot2::ggplot(aes(x=mod, y=obs)) +
      geom_hex() +
      scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="points") {

    ## points
    gg <- df %>%
      ggplot(aes(x=mod, y=obs)) +
      geom_point() +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="density") {

    ## points
    gg <- df %>%
      ggplot(aes(x=mod, y=obs)) +

      stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
      scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                           guide = "legend") +

      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  }

  return(list(df_metrics=df_metrics, gg=gg, linmod=linmod, results = results))
}

