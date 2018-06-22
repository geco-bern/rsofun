eval_response_neuralnet <- function( ddf ){

	require(caret)
	require(nnet)
	require(dplyr)
  
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

  ## predict values
  predicted <- predict( nn, df )
  
  save( nn, file file = "nn.Rdata" )

}