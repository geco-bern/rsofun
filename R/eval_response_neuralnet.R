eval_response_neuralnet <- function( df, overwrite = FALSE, ... ){

  ## rename (should go outside this function)
  df <- df %>% rename( vpd = vpd_fluxnet2015, ppfd = ppfd_fluxnet2015, soilm = soilm_obs_mean )
  
  ## filter out data if any of the variables is NA. Different for gpp_mod and gpp_obs
  df_training_obs <- filter( df, !is.na(gpp_obs) & !is.na(temp) & !is.na(vpd) & !is.na(fapar) & !is.na(ppfd) & !is.na(soilm) )
  df_training_mod <- filter( df, !is.na(gpp_mod) & !is.na(temp) & !is.na(vpd) & !is.na(fapar) & !is.na(ppfd) & !is.na(soilm) )
  
	## normalising data to within range
  preprocessParams <- preProcess( df, method=c("center", "scale") )

  ## settings for training
  traincotrlParams <- trainControl( method="repeatedcv", number=5, repeats=5, verboseIter=FALSE, p=0.75 ) # take best of 10 repetitions of training with 75% used for training (25% for testing)

  ## sample best number of hidden layers
  tune_grid <- expand.grid( .decay = c(0.1), .size = seq(4,20,2) )

  ## train the neural network at observed daily GPP
  filn <- "nn.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    nn <- train(
                as.formula( " gpp_obs ~ temp + vpd + fapar + ppfd + soilm " ),
                data      = df_training_obs, 
                method    = "nnet",
                linout    = TRUE,
                # tuneGrid  = tune_grid,
                preProc   = c("center", "scale"), # preprocessParams,
                trControl = traincotrlParams,
                trace     = TRUE
                )
    save( nn, file = filn )
  } else {
    load( filn )
  }

  ## train the neural network at modelled daily GPP
  filn <- "nn_mod.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    nn_mod <- train(
                as.formula( " gpp_mod ~ temp + vpd + fapar + ppfd + soilm " ),
                data      = df_training_mod, 
                method    = "nnet",
                linout    = TRUE,
                # tuneGrid  = tune_grid,
                preProc   = c("center", "scale"), # preprocessParams,
                trControl = traincotrlParams,
                trace     = TRUE
                )
    save( nn_mod, file = filn )
  } else {
    load( filn )
  }

  ## predict values
  predicted <- predict( nn, df_training_obs )
  df_training_obs <- df_training_obs %>% mutate( gpp_nn_obs = predicted[,1] )

  predicted <- predict( nn_mod, df_training_mod )
  df_training_mod <- df_training_mod %>% mutate( gpp_nn_mod = predicted[,1] )
  
  ## evaluate performance of NN-predictions
  stats_nn     <- with( df_training_obs,  analyse_modobs( gpp_nn_obs, gpp_obs, heat = TRUE, plot.fil = "fig/modobs_nn.pdf" ) )
  stats_nn_mod <- with( df_training_mod,  analyse_modobs( gpp_nn_mod, gpp_mod, heat = TRUE, plot.fil = "fig/modobs_nn_mod.pdf" ) )

  ##-------------------------------------
  ## Evaluate NN
  ##-------------------------------------
  ## temperature
  eval_response_byvar( df_training_obs, nn, nn_mod, evalvar = "temp", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 40, nsample = 12, ... )

  ## vpd
  eval_response_byvar( df_training_obs, nn, nn_mod, evalvar = "vpd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 3000, nsample = 12, ... )

  ## soilm
  eval_response_byvar( df_training_obs, nn, nn_mod, evalvar = "soilm", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12, ... )

  ## fapar
  eval_response_byvar( df_training_obs, nn, nn_mod, evalvar = "fapar", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 12, ... )

  ## ppfd
  eval_response_byvar( df_training_obs, nn, nn_mod, evalvar = "ppfd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 70, nsample = 12, ... )

}

eval_response_byvar <- function( df, nn, nn_mod, evalvar, predictors, varmin, varmax, nsample, makepdf=FALSE ){

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
  predicted_eval <- predict( nn, evaldata )
  evaldata <- evaldata %>% mutate( gpp_nn_obs = predicted_eval[,1] )

  predicted_eval <- predict( nn_mod, evaldata )
  evaldata <- evaldata %>% mutate( gpp_nn_mod = predicted_eval[,1] )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% group_by_( evalvar ) %>%
                           summarise( median = median( gpp_nn_obs ),  
                                      q33 = quantile( gpp_nn_obs, 0.33 ),
                                      q66 = quantile( gpp_nn_obs, 0.66 ),
                                      q25 = quantile( gpp_nn_obs, 0.25 ),
                                      q75 = quantile( gpp_nn_obs, 0.75 ),

                                      median_mod = median( gpp_nn_mod ),  
                                      q33_mod = quantile( gpp_nn_mod, 0.33 ),
                                      q66_mod = quantile( gpp_nn_mod, 0.66 ),
                                      q25_mod = quantile( gpp_nn_mod, 0.25 ),
                                      q75_mod = quantile( gpp_nn_mod, 0.75 )
                                      )

  ## plot
  if (makepdf) pdf( paste0("fig/nn_response_", evalvar, ".pdf") )

	  plot( eval_sum[[evalvar]], eval_sum$median, type = "l", col="black", ylim=c(0,10), xlab = evalvar, ylab = "GPP (gC m-2 d-1)" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33, rev(eval_sum$q66)), col=rgb(0,0,0,0.2), border = NA )

	  lines( eval_sum[[evalvar]], eval_sum$median_mod, col="red" )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_mod, rev(eval_sum$q66_mod)), col=rgb(1,0,0,0.2), border = NA )

  if (makepdf) dev.off()

}
