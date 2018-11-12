eval_response_gam <- function( df, overwrite = FALSE, ndays_agg = 5, ... ){

  ## xxx debug
  df <- out_eval_RED$data$ddf
  
  ## rename (should go outside this function)
  df <- df %>%  mutate( lue_obs = gpp_obs / (fapar * ppfd_fluxnet2015) ) %>%
                mutate( lue_obs = ifelse( is.nan(lue_obs), NA, lue_obs ) ) %>%
                mutate( lue_obs = remove_outliers(lue_obs) ) %>%
                dplyr::rename( vpd = vpd_fluxnet2015, ppfd = ppfd_fluxnet2015, soilm = soilm_obs_mean )
  
  ##------------------------------------------------------------
  ## Aggregate to multi-day periods
  ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
  ##------------------------------------------------------------
  ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
  listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )                  
  breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( ndays_agg, " days"), length.out = ceiling(365 / ndays_agg)) ) %>% Reduce(c,.)
  
  ## take mean across periods
  df <- df %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
               group_by( sitename, inbin ) %>%
               summarise( 
                  lue_obs_mean = mean( lue_obs, na.rm = TRUE ),
                  temp_mean    = mean( temp,    na.rm = TRUE ), 
                  vpd_mean     = mean( vpd,     na.rm = TRUE ), 
                  soilm_mean   = mean( soilm, na.rm   = TRUE ),
                  n_obs        = sum(!is.na(lue_obs)) 
                  ) %>%
               dplyr::rename( 
                  lue_obs = lue_obs_mean,
                  temp = temp_mean,
                  vpd = vpd_mean,
                  soilm = soilm_mean
                  ) %>%
               mutate( 
                  lue_obs = ifelse(is.nan(lue_obs), NA, lue_obs ),
                  temp = ifelse(is.nan(temp), NA, temp ),
                  vpd = ifelse(is.nan(vpd), NA, vpd ),
                  soilm = ifelse(is.nan(soilm), NA, soilm )
                  )


  ## filter out data if any of the variables is NA. Different for gpp_mod and gpp_obs
  df_training <- dplyr::filter( df, !is.na(lue_obs) & !is.na(temp) & !is.na(vpd) & !is.na(soilm) )
  
  ## filter days with temperature below zero
  df_training <- df_training %>% dplyr::filter( temp > 0.0 )
  
  ## train the neural network at observed daily GPP
  filn <- "tmpdir/gam.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam <- mgcv::gam( lue_obs ~ s(temp) + s(vpd) + s(soilm), data = df_training, method = "REML" )
    # gam.check(gam)
    # summary(gam)
    # plot(gam)
    save( gam, file = filn )
  } else {
    load( filn )
  }

  ## predict values with GAM
  predicted <- predict( gam, df_training )

  ## calculate values with P-model
  params_opt <- read_csv( "tmpdir/params_opt_RED.csv" )
  df_training <- df_training[1:3,] %>%  mutate( lue_gam = predicted[1:3] ) %>%
                                        mutate( lue_pmodel = rpmodel( tc = temp, vpd = vpd, co2 = 300, elv = 300, kphio = kphio params_opt$kphio, fapar = NA, ppfd = NA, method="full", returnvar = "lue" ) )
  
  # ## evaluate performance of gam-predictions
  # stats_gam      <- with( df_training,  analyse_modobs( lue_gam, gpp_obs, heat = TRUE, plot.fil = "fig/modobs_gam.pdf" ) )

  ##-------------------------------------
  ## Evaluate GAM
  ##-------------------------------------
  ## temperature
  eval_response_byvar( df_training, gam, evalvar = "temp", predictors = c("temp", "vpd", "soilm"), varmin = 0, varmax = 40, nsample = 12, ylim=c(0,0.5) ) 

  ## vpd
  eval_response_byvar( df_training, gam, evalvar = "vpd", predictors = c("temp", "vpd", "soilm"), varmin = 0, varmax = 3000, nsample = 12, ylim=c(0,0.5) )

  ## soilm
  eval_response_byvar( df_training, gam, evalvar = "soilm", predictors = c("temp", "vpd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12, ylim=c(0,0.5) )

}

eval_response_byvar <- function( df, gam, evalvar, predictors, varmin, varmax, nsample, makepdf=TRUE, ... ){

  if (evalvar %in% predictors) predictors <- predictors[-which(predictors==evalvar)]

  ## create synthetic data for predictors, sampling from the observed values independently for each predictor
  evaldata <- expand.grid(  seq( varmin, varmax, length.out = 30 ),
                            sample( unlist( df[ predictors[1] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[2] ] ), nsample, replace = TRUE )
                            ) %>% 
              as_tibble() %>%
              setNames( c( evalvar, predictors ) )

  ## predict with evaluation data
  predicted_eval <- predict( gam, evaldata )
  evaldata <- evaldata %>% mutate( lue_gam = predicted_eval )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% group_by_( evalvar ) %>%
                           summarise( median = median( lue_gam ),  
                                      q33 = quantile( lue_gam, 0.33 ),
                                      q66 = quantile( lue_gam, 0.66 ),
                                      q25 = quantile( lue_gam, 0.25 ),
                                      q75 = quantile( lue_gam, 0.75 )
                                      )

  ## plot response in observational and simulated data
  if (makepdf) pdf( paste0("fig/gam_response_", evalvar, ".pdf") )

    par(las=0)
	  plot( eval_sum[[evalvar]], eval_sum$median, type = "l", col="black", xlab = evalvar, ... )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33, rev(eval_sum$q66)), col=rgb(0,0,0,0.2), border = NA )

  if (makepdf) dev.off()

}
