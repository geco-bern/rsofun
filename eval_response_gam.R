eval_response_gam <- function( df, overwrite = FALSE, ... ){

	require(dplyr)
  require(mgcv)
  
  source("analyse_modobs.R")
  
  ## rename (should go outside this function)
  df <- df %>% dplyr::rename( vpd = vpd_fluxnet2015, ppfd = ppfd_fluxnet2015, soilm = soilm_obs_mean )
  
  ## filter out data if any of the variables is NA. Different for gpp_mod and gpp_obs
  df_training <- dplyr::filter( df, !is.na(gpp_mod) & !is.na(gpp_mod) & !is.na(gpp_obs) & !is.na(temp) & !is.na(vpd) & !is.na(fapar) & !is.na(ppfd) & !is.na(soilm) )
  
  ## train the neural network at observed daily GPP
  filn <- "gam_obs.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam_obs <- gam( gpp_obs ~ s(temp) + s(vpd) + s(fapar) + s(ppfd) + s(soilm), method = "REML" )
    save( gam_obs, file = filn )
  } else {
    load( filn )
  }

  ## train the neural network at modelled daily GPP
  filn <- "gam_mod.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    gam_mod <- gam( gpp_mod ~ s(temp) + s(vpd) + s(fapar) + s(ppfd) + s(soilm), method = "REML" )
    save( gam_mod, file = filn )
  } else {
    load( filn )
  }

  ## predict values
  predicted <- predict( gam_obs, df_training )
  df_training <- df_training %>% mutate( gpp_gam_obs = predicted[,1] )

  predicted <- predict( gam_mod, df_training )
  df_training <- df_training %>% mutate( gpp_gam_mod = predicted[,1] )
  
  ## evaluate performance of gam-predictions
  stats_gam     <- with( df_training,  analyse_modobs( gpp_gam_obs, gpp_obs, heat = TRUE, plot.fil = "fig/modobs_gam.pdf" ) )
  stats_gam_mod <- with( df_training,  analyse_modobs( gpp_gam_mod, gpp_mod, heat = TRUE, plot.fil = "fig/modobs_gam_mod.pdf" ) )

  ##-------------------------------------
  ## Evaluate gam
  ##-------------------------------------
  ## temperature
  eval_response_byvar( df_training, gam_obs, gam_mod, evalvar = "temp", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 40, nsample = 12, ... )

  ## vpd
  eval_response_byvar( df_training, gam_obs, gam_mod, evalvar = "vpd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 3000, nsample = 12, ... )

  ## soilm
  eval_response_byvar( df_training, gam_obs, gam_mod, evalvar = "soilm", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12, ... )

  ## fapar
  eval_response_byvar( df_training, gam_obs, gam_mod, evalvar = "fapar", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12, ... )

  ## ppfd
  eval_response_byvar( df_training, gam_obs, gam_mod, evalvar = "ppfd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 70, nsample = 12, ... )

}

eval_response_byvar <- function( df, gam_obs, gam_mod, evalvar, predictors, varmin, varmax, nsample, makepdf=FALSE ){

  if (evalvar %in% predictors) predictors <- predictors[-which(predictors==evalvar)]

  ## create synthetic data for predictors, sampling from the observed values independently for each predictor
  evaldata <- expand.grid(  seq( varmin, varmax, length.out = 50 ),
                            sample( unlist( df[ predictors[1] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[2] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[3] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[4] ] ), nsample, replace = TRUE ) 
                            ) %>% 
              as_tibble() %>%
              setNames( c( evalvar, predictors ) )

  ## predict with evaluation data
  predicted_eval <- predict( gam_obs, evaldata )
  evaldata <- evaldata %>% mutate( gpp_gam_obs = predicted_eval[,1] )

  predicted_eval <- predict( gam_mod, evaldata )
  evaldata <- evaldata %>% mutate( gpp_gam_mod = predicted_eval[,1] )

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
                                      q75_mod = quantile( gpp_gam_mod, 0.75 )
                                      )

  ## plot
  if (makepdf) pdf( paste0("fig/gam_response_", evalvar, ".pdf") )

	  plot( eval_sum[[evalvar]], eval_sum$median, type = "l", col="black", ylim=c(0,10), xlab = evalvar, ylab = "GPP (gC m-2 d-1)" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33, rev(eval_sum$q66)), col=rgb(0,0,0,0.2), border = NA )

	  lines( eval_sum[[evalvar]], eval_sum$median_mod, col="red" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_mod, rev(eval_sum$q66_mod)), col=rgb(1,0,0,0.2), border = NA )

  if (makepdf) dev.off()

}
