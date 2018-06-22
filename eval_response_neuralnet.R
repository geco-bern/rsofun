eval_response_neuralnet <- function( ddf, overwrite = FALSE ){

	require(caret)
	require(nnet)
	require(dplyr)
  
  source("analyse_modobs.R")
  
  ## rename (should go outside this function)
  df <- ddf %>% rename( vpd = vpd_fluxnet2015, ppfd = ppfd_fluxnet2015, soilm = soilm_obs_mean )
  
	## clean data
	df <- df %>% filter( !is.na(gpp_obs) & !is.na(temp) & !is.na(vpd) & !is.na(fapar) & !is.na(ppfd) & !is.na(soilm) )

	## model formula
	forml <- as.formula( " gpp_obs ~ temp + vpd + fapar + ppfd + soilm " )

	## normalising data to within range
  preprocessParams <- preProcess( df, method=c("center", "scale") )

  ## settings for training
  traincotrlParams <- trainControl( method="repeatedcv", number=5, repeats=5, verboseIter=FALSE, p=0.75 ) # take best of 10 repetitions of training with 75% used for training (25% for testing)

  ## sample best number of hidden layers
  tune_grid <- expand.grid( .decay = c(0.1), .size = seq(4,20,2) )

  ## train the neural network
  filn <- "nn.Rdata"
  if (!file.exists(filn)||overwrite){
    set.seed(1982)
    nn <- train(
                forml,
                data      = df, #training,
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

  ## predict values
  predicted <- predict( nn, df )
  df <- df %>% mutate( gpp_nn = predicted[,1] )
  
  ## evaluate performance of NN-predictions
  stats_nn <- with( df,  analyse_modobs( gpp_nn, gpp_obs, heat = TRUE, plot.fil = "fig/modobs_nn.pdf" ) )

  ##-------------------------------------
  ## Evaluate NN
  ##-------------------------------------
  ## temperature
  nsample <- 10
  eval_response_byvar( df, evalvar = "temp", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 40, nsample = 10, pdfname = "fig/nn_response_temp.pdf" )

  ## vpd
  eval_response_byvar( df, evalvar = "vpd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 3000, nsample = 10, pdfname = "fig/nn_response_vpd.pdf" )

  ## soilm
  eval_response_byvar( df, evalvar = "soilm", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 10, pdfname = "fig/nn_response_soilm.pdf" )

  ## fapar
  eval_response_byvar( df, evalvar = "fapar", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 1.0, nsample = 10, pdfname = "fig/nn_response_fapar.pdf" )

  ## ppfd
  eval_response_byvar( df, evalvar = "ppfd", predictors = c("temp", "vpd", "fapar", "ppfd", "soilm"), varmin = 0, varmax = 70, nsample = 10, pdfname = "fig/nn_response_ppfd.pdf" )

}

eval_response_byvar <- function( df, evalvar, predictors, varmin, varmax, nsample, pdfname=NA ){

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
  evaldata <- evaldata %>% mutate( gpp_nn = predicted_eval[,1] )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% group_by_( evalvar ) %>%
                           summarise( median = median( gpp_nn ),  
                                      q33 = quantile( gpp_nn, 0.33 ),
                                      q66 = quantile( gpp_nn, 0.66 ),
                                      q25 = quantile( gpp_nn, 0.25 ),
                                      q75 = quantile( gpp_nn, 0.75 )
                                      )
                           
  ## plot
  if(!is.na(pdfname)) pdf( pdfname )
	  plot( eval_sum[[evalvar]], eval_sum$median, type = "l", col="red", ylim=c(0,10), xlab = evalvar, ylab = "GPP (gC m-2 d-1)")
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q25, rev(eval_sum$q75)), col=rgb(0,0,0,0.2), border = NA )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33, rev(eval_sum$q66)), col=rgb(0,0,0,0.2), border = NA )
  if(!is.na(pdfname)) dev.off()
}
