gapfill_nn <- function( data, predictors, nam_target, package="neuralnet" ){
  ##--------------------------------------------------------------------
  ## Gap-fill using NN 
  ##
  ## Arguments:
  ## data: data frame with gaps in column corresponding to nam_target
  ## predictors: name of columns in 'data' used as input to the NN
  ## nam_target: name of the column in 'data' used as target for the NN training
  ##--------------------------------------------------------------------

  if (package=="neuralnet"){
    ##--------------------------------------------------------------------
    ## Using package neuralnet
    ##--------------------------------------------------------------------
    library( neuralnet )

    ## identify NAs
    na_idxs <- which( is.na( data$le_f_mds ) )
    
    ## for training use data with NAs removed
    train <- data[ -na_idxs, ]

    ## scale variables to within [0,1]
    maxs       <- apply(train, 2, max) 
    mins       <- apply(train, 2, min)
    train_     <- as.data.frame( scale( train, center = mins, scale = maxs - mins ) )
    data_      <- as.data.frame( scale( data,  center = mins, scale = maxs - mins ) )

    ## train the NN
    forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )

    nn <- neuralnet( forml, data=train_, hidden=6, linear.output=TRUE, lifesign="full", threshold=0.02, rep=1 )
    # nn <- repeat_neuralnet( forml, train_, test_, predictors, hidden=6, linear.output=TRUE, lifesign=lifesign, threshold=threshold, rep=nrep )

    ## predicting
    pred_nn_ <- try( compute( nn, subset( data_, select=predictors ) ) )

    if (class(pred_nn_)!="try-error"){
      
      ## scaling back
      range   <- max( train[[ nam_target ]] ) - min( train[[ nam_target ]] )
      offset  <- min( train[[ nam_target ]] )
      pred_nn <- pred_nn_$net.result * range + offset

    }

    # ## plot for verification
    # plot( seq(nrow(data)), data[[ nam_target]], type="l" ) ## with gaps
    # # lines( na_idxs, pred_nn[na_idxs], col='red' )  ## gap-filled
    # lines( seq(nrow(data)), pred_nn, col='red' )  ## gap-filled

    ## fill gaps
    data[[ nam_target ]][ na_idxs ] <- pred_nn[ na_idxs ]


  } else if (package=="caret"){
    ##--------------------------------------------------------------------
    ## Using package caret
    ##--------------------------------------------------------------------    
    .libPaths( c( .libPaths(), "/home/bstocker/R/x86_64-pc-linux-gnu-library/3.3") )

    require( caret )
    require( nnet )

    ## some predictors may be NA. Approximate by linear interpolation.
    for (ipred in predictors){
      ## if first value is NA, fill with second
      if ( is.na(data[[ ipred ]][1]) ) { data[[ ipred ]][1] <- data[[ ipred ]][2] }

      ## if last value is NA then fill with second-last
      if ( is.na(data[[ ipred ]][nrow(data)]) ) { data[[ ipred ]][nrow(data)] <- data[[ ipred ]][nrow(data)-1] }

      ## fill NAs in between time series
      data[[ ipred ]][ which(is.na(data[[ ipred ]])) ] <- approx( data$year_dec, data[[ ipred ]] )$y[ which(is.na(data[[ ipred ]])) ]

    }
    # print(apply(data, 2, FUN = function (x) sum(is.na(x))))

    ## this has caused a problem before due to values being too hight -> weird
    data[[ nam_target ]] <- data[[ nam_target ]] * 1e-6

    ## identify NAs
    na_idxs <- which( is.na( data[[ nam_target ]] ) )

    ## for training use data with NAs removed
    train <- data[ -na_idxs, ]

    preprocessParams <- preProcess( train, method=c("range") )
    traincotrlParams <- trainControl( method="repeatedcv", number=10, repeats=3, verboseIter=FALSE, p=0.75 ) # take best of 10 repetitions of training with 75% used for training (25% for testing)

    forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
    nn <- train(
                forml,
                data      = train, #training,
                method    = "nnet",
                linout    = TRUE,
                tuneGrid  = expand.grid( .decay = c(1e-3), .size = c(20) ),
                preProc   = "range", # c("center", "scale"), # "range", # preProc  = preprocessParams
                trControl = traincotrlParams,
                trace     = FALSE
                )

    pred_nn <- as.vector( predict( nn, data ))   
    # pred_nn <- pred_nn * 1e6 
    
    # ## plot for verification
    # plot( seq(nrow(data)), data[[ nam_target]], type="n" ) ## with gaps
    # lines( seq(nrow(data)), pred_nn, col='red' )  ## gap-filled
    # lines( seq(nrow(data)), data[[ nam_target]] )  ## with gaps

    ## fill gaps
    data[[ nam_target ]][ na_idxs ] <- pred_nn[ na_idxs ]

    ## revert
    data[[ nam_target ]] <- data[[ nam_target ]] * 1e6

  }

  return( data )

}
