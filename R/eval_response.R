#' Evaluates the functional relationships.
#'
#' Evaluates the functional relationships in the data using General Additive Models (using function \code{gam} from the \code{mgcv} package) and the P-model using its function.
#' 
#' @param df A data frame containing daily observational data as output from function \code{get_obs_eval()}.
#' @param overwrite if \code{TRUE}, the GAM model object is overwritten
#' @param ndays_agg An integer specifying the level of data aggregation by number of days.
#'
#' @return A data frame with aggregated data including GAM predictions and P-model results as columns \code{lue_gam} and \code{lue_mod}, respectively.
#' @export
#'
#' @examples eval_response( df, overwrite = TRUE, ndays_agg = 10 )
#' 
eval_response <- function( df, overwrite = FALSE, ndays_agg = 10, ... ){

  # ## xxx debug
  # df <- out_eval_RED$data$ddf
  
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
  df_agg <- df %>%  mutate( inbin = cut( date, breaks = breaks, right = FALSE ) ) %>%
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
                      temp    = temp_mean,
                      vpd     = vpd_mean,
                      soilm   = soilm_mean
                      ) %>%
                    mutate( 
                      lue_obs = ifelse(is.nan(lue_obs), NA, lue_obs ),
                      temp    = ifelse(is.nan(temp), NA, temp ),
                      vpd     = ifelse(is.nan(vpd), NA, vpd ),
                      soilm   = ifelse(is.nan(soilm), NA, soilm )
                      ) %>%
                    mutate( date = ymd(as.character(inbin)) ) %>%
                    dplyr::select( -inbin ) %>%
                    ungroup()

  ## filter out data if any of the variables is NA. Different for gpp_mod and gpp_obs
  df_training <- dplyr::filter( df_agg, !is.na(lue_obs) & !is.na(temp) & !is.na(vpd) & !is.na(soilm) )
  
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
  params_opt <- readr::read_csv( "tmpdir/params_opt_RED.csv" )
  df_training <- df_training %>%  mutate( lue_gam = predicted ) %>%
    group_by( date, sitename ) %>%
    nest() %>%
    mutate( out_pmodel = purrr::map( data, ~rpmodel(  tc = .$temp, 
                                                      vpd = .$vpd, 
                                                      co2 = 300, 
                                                      elv = 300, 
                                                      kphio = params_opt$kphio, 
                                                      fapar = NA, 
                                                      ppfd = NA, 
                                                      method="full"
                                                      ) ) ) %>%
    mutate( lue_mod = purrr::map_dbl( out_pmodel, "lue" ) ) %>%
    unnest( data )
              
  ## xxx test
  # with( filter(df, sitename=="FR-LBr"), plot( date, lue_obs, type="l", ylim=c(0,1)))
  # with( filter(df_agg, sitename=="FR-LBr"), lines( date, lue_obs, col="red"))
  # with( filter(df_training, sitename=="FR-LBr"), lines( date, lue_gam, col="green"))
  # with( filter(df_training, sitename=="FR-LBr"), lines( date, lue_mod, col="cyan"))

  # ## evaluate performance of GAM and P-model predictions
  # stats_gam <- with( df_training,  analyse_modobs( lue_gam, lue_obs, heat = TRUE ) )
  # stats_mod <- with( df_training,  analyse_modobs( lue_mod, lue_obs, heat = TRUE ) )
  
  ##-------------------------------------
  ## Evaluate GAM and P-model
  ##-------------------------------------
  ## temperature
  eval_response_byvar( df_training, gam, evalvar = "temp", predictors = c("temp", "vpd", "soilm"), kphio = params_opt$kphio, varmin = 0, varmax = 40, nsample = 12, ylim=c(0,0.5) ) 

  ## vpd
  eval_response_byvar( df_training, gam, evalvar = "vpd", predictors = c("temp", "vpd", "soilm"), kphio = params_opt$kphio, varmin = 0, varmax = 3000, nsample = 12, ylim=c(0,0.5) )

  ## soilm
  eval_response_byvar( df_training, gam, evalvar = "soilm", predictors = c("temp", "vpd", "soilm"), kphio = params_opt$kphio, varmin = 0, varmax = 1.0, nsample = 12, ylim=c(0,0.5) )

  ## Return aggregated data with GAM predictions and P-model results
  df_agg <- df_agg %>% left_join( dplyr::select( df_training, lue_gam, lue_mod ), by = c("sitename", "date") )

  return(df_agg)

}

eval_response_byvar <- function( df, gam, evalvar, predictors, kphio, varmin, varmax, nsample, makepdf=TRUE, ... ){

  if (evalvar %in% predictors) predictors <- predictors[-which(predictors==evalvar)]

  ## create synthetic data for predictors, sampling from the observed values independently for each predictor
  evaldata <- expand.grid(  seq( varmin, varmax, length.out = 30 ),
                            sample( unlist( df[ predictors[1] ] ), nsample, replace = TRUE ), 
                            sample( unlist( df[ predictors[2] ] ), nsample, replace = TRUE )
                            ) %>% 
              as_tibble() %>%
              setNames( c( evalvar, predictors ) )

  ## predict for GAM with evaluation data
  predicted_eval <- predict( gam, evaldata )
  evaldata <- evaldata %>% mutate( lue_gam = predicted_eval )

  ## predict for P-model with evaluation data
  evaldata <- evaldata %>%  mutate( id = 1:n() ) %>%
                            group_by( id ) %>%
                            nest() %>%
                            mutate( out_pmodel = purrr::map( data, ~rpmodel(  tc = .$temp, 
                                                                              vpd = .$vpd, 
                                                                              co2 = 300, 
                                                                              elv = 300, 
                                                                              kphio = kphio, 
                                                                              fapar = NA, 
                                                                              ppfd = NA, 
                                                                              method="full"
                                                                              ) ) ) %>%
                            mutate( lue_mod = purrr::map_dbl( out_pmodel, "lue" ) ) %>%
                            unnest( data )

  ## summarise by temperature steps
  eval_sum <- evaldata %>% group_by_( evalvar ) %>%
                           summarise( median_gam = median( lue_gam ),  
                                      q33_gam = quantile( lue_gam, 0.33 ),
                                      q66_gam = quantile( lue_gam, 0.66 ),
                                      q25_gam = quantile( lue_gam, 0.25 ),
                                      q75_gam = quantile( lue_gam, 0.75 ),

                                      median_mod = median( lue_mod ),  
                                      q33_mod = quantile( lue_mod, 0.33 ),
                                      q66_mod = quantile( lue_mod, 0.66 ),
                                      q25_mod = quantile( lue_mod, 0.25 ),
                                      q75_mod = quantile( lue_mod, 0.75 )
                                      )

  ## plot response in observational and simulated data
  if (makepdf) filn <- paste0("fig/gam_response_", evalvar, ".pdf")
  if (makepdf) inform( paste( "Creating plot", filn ))
  if (makepdf) pdf( filn )

    par(las=0)
	  plot( eval_sum[[evalvar]], eval_sum$median_gam, type = "l", col="black", xlab = evalvar, ... )
	  polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_gam, rev(eval_sum$q66_gam)), col=rgb(0,0,0,0.2), border = NA )

    lines( eval_sum[[evalvar]], eval_sum$median_mod, col="red" )
    polygon( c(eval_sum[[evalvar]], rev(eval_sum[[evalvar]])), c(eval_sum$q33_mod, rev(eval_sum$q66_mod)), col=rgb(1,0,0,0.2), border = NA )

  if (makepdf) dev.off()

}
