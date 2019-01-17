#' Evaluates the functional relationships.
#'
#' Evaluates the functional relationships in the data using General Additive Models (using function \code{gam} from the \code{mgcv} package).
#' 
#' @param df A data frame containing daily observational data as output from function \code{get_obs_eval()}.
#' @param nam_target A character string specifying the column name of the target variable for which the functional relationships are to be determined.
#' @param predictors A vector of character strings specifying the statistical model predictors by column name in the training data (argument \code{df}).
#' @param dir A character string specifying the path of the directory into which the GAM model object (named \code{gam.Rdata}) is written. Defaults to \code{"./"} 
#' @param overwrite If \code{TRUE}, the GAM model object is overwritten
#' @param nsample_target Length of the equally spaced sequence of values of the target variable given by \code{nam_target}.
#' @param nsample_predictors Number of randomly drawn values of the predictors, given by \code{predictors}.
#'
#' @return A named list of data frames with aggregated data including GAM predictions as columns \code{*_gam}. The elements of the list are the evaluation datasets with GAM-predicted values for each predictor.
#' @export
#'
#' @examples eval_response( df, overwrite = TRUE )
#' 
eval_functionalrel <- function( df, nam_target, predictors, dir = "./", overwrite = FALSE, nsample_target=30, nsample_predictors=30, ... ){

  # ## elegantly dropping rows containing NA in nam_target or predictors columns
  # df_training <- df %>% dplyr::drop_na( c(nam_target, predictors) ) %>% 

  # not-so elegantly dropping NA rows
  df_training <- df %>% dplyr::filter_at( vars(one_of(c(nam_target, predictors))), all_vars(!is.na(.)) ) %>% 
  
    ## filter days with temperature below zero
    dplyr::filter( temp > 0.0 )

  ## train the neural network at observed daily GPP
  if (!file.exists( paste0(dir, "/gam.Rdata"))||overwrite){
    set.seed(1982)
    
    ## create formula with splines for each predictor "s(predictor)"
    forml  <- as.formula(  paste0( nam_target, " ~ s(", paste( predictors, collapse=") + s(" ), ")" ) )
    
    ## train model
    gam <- mgcv::gam( forml, data = df_training, method = "REML" )

    # gam.check(gam)
    # summary(gam)
    # plot(gam)
    save( gam, file =  paste0(dir, "/gam.Rdata") )
  } else {
    load( gam, file =  paste0(dir, "/gam.Rdata") )
 }

  ## predict values with GAM
  predicted     <- predict( gam, df_training )

  ##-------------------------------------
  ## Evaluate GAM and P-model
  ##-------------------------------------
  df_eval <- purrr::map( 
    as.list(predictors), 
    ~eval_response_byvar( ., 
      df_training, 
      gam, 
      all_predictors = predictors, 
      nam_target = nam_target, 
      nsample_predictors = nsample_predictors, 
      nsample_target = nsample_target 
      ) )
  names(df_eval) <- predictors

  return(df_eval)

}

eval_response_byvar <- function( evalvar, df, gam, all_predictors, nam_target, nsample_predictors, nsample_target, makepdf=FALSE, ... ){

  ## Get evaluation data
  evaldata <- get_eval_data( evalvar, df, all_predictors, nsample_predictors, nsample_target )

  ## predict for GAM with evaluation data
  predicted_eval <- predict( gam, evaldata )
  evaldata <- evaldata %>% mutate( var_gam = predicted_eval )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% get_quantiles( evalvar, nam_target = "var_gam" )
  names(eval_sum) <- paste0( names(eval_sum), "_gam" )

  return( eval_sum )

}


## aggregates to multi-day periods
#' @param ndays_agg An integer specifying the level of data aggregation by number of days. If \code{ndays_agg=NULL}, no aggregation is done.
aggregate_mean <- function( ddf, ndays_agg, dovars, year_start, year_end ){

  if (ndays_agg==1){

    return(ddf)

  } else {

    ## Generate periods: vector of starting dates of X-day periods, making sure the 1st of Jan is always the start of a new period
    listyears <- seq( ymd( paste0( year_start, "-01-01" ) ), ymd( paste0( year_end, "-01-01" ) ), by = "year" )                  
    breaks <- purrr::map( as.list(listyears), ~seq( from=., by=paste0( ndays_agg, " days"), length.out = ceiling(365 / ndays_agg)) ) %>% Reduce(c,.)
    
    ## take mean across periods and repalce NaN by NA
    ddf_agg <- ddf %>%  mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
      group_by( sitename, inbin ) %>%
      summarise_at( vars(one_of(dovars)), funs(mean(., na.rm=TRUE) )) %>%
      mutate_at( vars(one_of(dovars)), funs(ifelse(is.nan(.), NA, .)))  %>%
      mutate( date = ymd(as.character(inbin)) ) %>%
      dplyr::select( -inbin ) %>%
      ungroup()
    return(ddf_agg)
  
  }  
}


get_eval_data <- function( evalvar, df, all_predictors, nsample_predictors, nsample_target ){

  if (evalvar %in% all_predictors){
    predictors <- all_predictors[-which(all_predictors==evalvar)]
  } else {
    predictors <- all_predictors
  }

  varmin <- min( df[[evalvar]], na.rm=TRUE )
  varmax <- max( df[[evalvar]], na.rm=TRUE )

  ## create synthetic data for predictors, sampling from the observed values independently for each predictor
  evaldata <- expand.grid(  seq( varmin, varmax, length.out = nsample_target ),
                            sample( unlist( df[ predictors[1] ] ), nsample_predictors, replace = TRUE ), 
                            sample( unlist( df[ predictors[2] ] ), nsample_predictors, replace = TRUE )
                            ) %>% 
              as_tibble() %>%
              setNames( c( evalvar, predictors ) )

  return(evaldata)
}

get_quantiles <- function( df, evalvar, nam_target ){

  ## summarise by temperature steps
  df_agg <- df %>% group_by_( evalvar ) %>%
                   summarise( 
                              median = median(    eval(as.name(nam_target)) ),  
                              q33    = quantile(  eval(as.name(nam_target)), 0.33 ),
                              q66    = quantile(  eval(as.name(nam_target)), 0.66 ),
                              q25    = quantile(  eval(as.name(nam_target)), 0.25 ),
                              q75    = quantile(  eval(as.name(nam_target)), 0.75 )
                              ) %>%
                   mutate( evalvar=evalvar )
  return(df_agg)
}

