eval_response_gam <- function( df, overwrite = FALSE, ndays_agg = 5, ... ){

	require(dplyr)
  require(mgcv)
  
  source("analyse_modobs.R")
  
  ## rename (should go outside this function)
  df <- df %>%  dplyr::rename( vpd = vpd_fluxnet2015, ppfd = ppfd_fluxnet2015, soilm = soilm_obs_mean ) %>%
                mutate( bias = gpp_mod - gpp_obs )
  
  # ##------------------------------------------------------------
  # ## Aggregate to multi-day periods
  # ## periods should start with the 1st of January each year, otherwise can't compute mean seasonal cycle
  # ##------------------------------------------------------------
  # ## Generate vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
  # listyears <- seq( ymd("1990-01-01"), ymd("2018-01-01"), by = "year" )                  
  # breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( ndays_agg, " days"), length.out = ceiling(365 / ndays_agg)) ) %>% Reduce(c,.)
  # 
  # ## take mean across periods
  # df <- df %>% mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
  #              group_by( sitename, inbin ) %>%
  #              summarise( 
  #                 gpp_obs_mean = mean( gpp_obs, na.rm = TRUE ),
  #                 gpp_mod_mean = mean( gpp_mod, na.rm = TRUE ), 
  #                 temp_mean    = mean( temp,    na.rm = TRUE ), 
  #                 vpd_mean     = mean( vpd,     na.rm = TRUE ), 
  #                 fapar_mean   = mean( fapar, na.rm   = TRUE ), 
  #                 ppfd_mean    = mean( ppfd, na.rm    = TRUE ), 
  #                 soilm_mean   = mean( soilm, na.rm   = TRUE ),
  #                 bias_mean    = mean( bias, na.rm   = TRUE ),
  #                 n_obs        = sum(!is.na(gpp_obs)) 
  #                 ) %>%
  #              dplyr::rename( 
  #                 gpp_obs = gpp_obs_mean,
  #                 gpp_mod = gpp_mod_mean,
  #                 temp = temp_mean,
  #                 vpd = vpd_mean,
  #                 fapar = fapar_mean,
  #                 ppfd = ppfd_mean,
  #                 soilm = soilm_mean,
  #                 bias = bias_mean
  #                 ) %>%
  #              mutate( 
  #                 gpp_obs = ifelse(is.nan(gpp_obs), NA, gpp_obs ),
  #                 gpp_mod = ifelse(is.nan(gpp_mod), NA, gpp_mod ),
  #                 temp = ifelse(is.nan(temp), NA, temp ),
  #                 vpd = ifelse(is.nan(vpd), NA, vpd ),
  #                 fapar = ifelse(is.nan(fapar), NA, fapar ),
  #                 ppfd = ifelse(is.nan(ppfd), NA, ppfd ),
  #                 bias = ifelse(is.nan(bias), NA, bias ),
  #                 soilm = ifelse(is.nan(soilm), NA, soilm )
  #                 )


  ## filter out data if any of the variables is NA. Different for gpp_mod and gpp_obs
  df_training <- dplyr::filter( df, !is.na(bias) & !is.na(gpp_mod) & !is.na(gpp_obs) & !is.na(temp) & !is.na(vpd) & !is.na(fapar) & !is.na(ppfd) & !is.na(soilm) )
  
  ## filter days with temperature below zero
  df_training <- df_training %>% dplyr::filter( temp > 0.0 )
  
  ## train the neural network at observed daily GPP
  filn <- "gam_obs.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam_obs <- gam( gpp_obs ~ s(temp) + s(vpd) + s(fapar) + s(ppfd) + s(soilm), data = df_training, method = "REML" )
    gam.check(gam_obs)
    summary(gam_obs)
    plot(gam_obs)
    save( gam_obs, file = filn )
  } else {
    load( filn )
  }

  ## train the neural network at modelled daily GPP
  filn <- "gam_mod.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam_mod <- gam( gpp_mod ~ s(temp) + s(vpd) + s(fapar) + s(ppfd) + s(soilm), data = df_training, method = "REML" )
    gam.check(gam_mod)
    summary(gam_mod)
    plot(gam_mod)
    save( gam_mod, file = filn )
  } else {
    load( filn )
  }

  ## train the neural network at modelled daily GPP
  filn <- "gam_bias.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam_bias <- gam( bias ~ s(temp) + s(vpd) + s(fapar) + s(ppfd) + s(soilm), data = df_training, method = "REML" )
    gam.check(gam_bias)
    summary(gam_bias)
    plot(gam_bias)
    save( gam_bias, file = filn )
  } else {
    load( filn )
  }

  ## predict values
  predicted <- predict( gam_obs, df_training )
  df_training <- df_training %>% mutate( gpp_gam_obs = predicted )

  predicted <- predict( gam_mod, df_training )
  df_training <- df_training %>% mutate( gpp_gam_mod = predicted )

  predicted <- predict( gam_bias, df_training )
  df_training <- df_training %>% mutate( bias_gam = predicted )
  
  # ## evaluate performance of gam-predictions
  # stats_gam      <- with( df_training,  analyse_modobs( gpp_gam_obs, gpp_obs, heat = TRUE, plot.fil = "fig/modobs_gam.pdf" ) )
  # stats_gam_mod  <- with( df_training,  analyse_modobs( gpp_gam_mod, gpp_mod, heat = TRUE, plot.fil = "fig/modobs_gam_mod.pdf" ) )
  # stats_gam_bias <- with( df_training,  analyse_modobs( gpp_gam_bias, gpp_mod, heat = TRUE, plot.fil = "fig/modobs_gam_bias.pdf" ) )

  ##-------------------------------------
  ## Evaluate GAM-GPP and bias
  ##-------------------------------------
  ## temperature
  eval_response_byvar( df_training, gam_obs, gam_mod, gam_bias, evalvar = "temp", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 40, nsample = 12 )

  ## vpd
  eval_response_byvar( df_training, gam_obs, gam_mod, gam_bias, evalvar = "vpd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 3000, nsample = 12 )

  ## soilm
  eval_response_byvar( df_training, gam_obs, gam_mod, gam_bias, evalvar = "soilm", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12 )

  ## fapar
  eval_response_byvar( df_training, gam_obs, gam_mod, gam_bias, evalvar = "fapar", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12 )

  ## ppfd
  eval_response_byvar( df_training, gam_obs, gam_mod, gam_bias, evalvar = "ppfd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 70, nsample = 12 )

}

eval_response_byvar <- function( df, gam_obs, gam_mod, gam_bias, evalvar, predictors, varmin, varmax, nsample, makepdf=TRUE, ... ){

  if (evalvar %in% predictors) predictors <- predictors[-which(predictors==evalvar)]

  ## create synthetic data for predictors, sampling from the observed values independently for each predictor
  evaldata <- expand.grid(  seq( varmin, varmax, length.out = 30 ),
                            sample( unlist( df[ predictors[1] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[2] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[3] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[4] ] ), nsample, replace = TRUE ) 
                            ) %>% 
              as_tibble() %>%
              setNames( c( evalvar, predictors ) )

  ## predict with evaluation data
  predicted_eval <- predict( gam_obs, evaldata )
  evaldata <- evaldata %>% mutate( gpp_gam_obs = predicted_eval )

  predicted_eval <- predict( gam_mod, evaldata )
  evaldata <- evaldata %>% mutate( gpp_gam_mod = predicted_eval )

  predicted_eval <- predict( gam_bias, evaldata )
  evaldata <- evaldata %>% mutate( bias_gam = predicted_eval )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% group_by_( evalvar ) %>%
                           summarise( median = median( gpp_gam_obs ),  
                                      q33 = quantile( gpp_gam_obs, 0.33 ),
                                      q66 = quantile( gpp_gam_obs, 0.66 ),
                                      q25 = quantile( gpp_gam_obs, 0.25 ),
                                      q75 = quantile( gpp_gam_obs, 0.75 ),

                                      median_mod = median( gpp_gam_mod ),  
                                      q33_mod = quantile( gpp_gam_mod, 0.33 ),
                                      q66_mod = quantile( gpp_gam_mod, 0.66 ),
                                      q25_mod = quantile( gpp_gam_mod, 0.25 ),
                                      q75_mod = quantile( gpp_gam_mod, 0.75 ),


                                      median_bias = median( bias_gam ),  
                                      q33_bias = quantile( bias_gam, 0.33 ),
                                      q66_bias = quantile( bias_gam, 0.66 ),
                                      q25_bias = quantile( bias_gam, 0.25 ),
                                      q75_bias = quantile( bias_gam, 0.75 )
                                      )

  ## plot response in observational and simulated data
  if (makepdf) pdf( paste0("fig/gam_response_", evalvar, ".pdf") )

	  plot( eval_sum[[evalvar]], eval_sum$median, type = "l", col="black", ylim=c(-2,10), xlab = evalvar, ylab = "GPP (gC m-2 d-1)" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33, rev(eval_sum$q66)), col=rgb(0,0,0,0.2), border = NA )

	  lines( eval_sum[[evalvar]], eval_sum$median_mod, col="red" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_mod, rev(eval_sum$q66_mod)), col=rgb(1,0,0,0.2), border = NA )

  if (makepdf) dev.off()

  ## plot response in bias between modelled and observed
  if (makepdf) pdf( paste0("fig/gam_bias_", evalvar, ".pdf") )

    plot( eval_sum[[evalvar]], eval_sum$median_bias, type = "l", col="black", xlab = evalvar, ylab = "GPP (gC m-2 d-1)" )
    polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_bias, rev(eval_sum$q66_bias)), col=rgb(0,0,0,0.2), border = NA )

  if (makepdf) dev.off()

}
