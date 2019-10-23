#' Conducts an out-of-bag calibration
#'
#' Wraps around calib_sofun() to calibrate out-of-bag (with one left-out site) and returns the evaluation result 
#' done for the left-out site.
#' 
#' @param setup A list containging the model settings. See vignette_rsofun.pdf for more information and an example.
#' @param settings_calib A list containing model calibration settings. See vignette_rsofun.pdf for more information and examples.
#' @param settings_eval A list specifying evaluation settings 
#' (see vignette eval_sofun.pdf for more information and examples)
#' @param settings_sims A list containing model simulation settings from \code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more information and examples.
#' @param settings_input A list containing model input settings. See vignette_rsofun.pdf for more information and examples.
#' @param ddf_obs_calib A data frame containing observational data used for model calibration. Created by function \code{get_obs_calib()}
#' @param ddf_obs_eval A data frame containing observational data used for model evaluation Created by function \code{get_obs_eval()}
#'
#' @return A nested list of objects returned by \code{\link{eval_sofun}}.
#' @export
#'
#' @examples xxx
#' 
oob_calib_eval_sofun <- function( setup, settings_calib, settings_eval, settings_sims, settings_input, ddf_obs_calib, ddf_obs_eval ){

  ## Get list of results from out-of-bag calibration 
  out_oob <- purrr::map(
    as.list(settings_calib$sitenames),
    ~oob_calib_eval_sofun_bysite(., 
                                 setup, 
                                 settings_calib, 
                                 settings_eval, 
                                 settings_sims, 
                                 settings_input, 
                                 ddf_obs_calib = ddf_obs_calib, 
                                 ddf_obs_eval = ddf_obs_eval
                                 )
    )
  names(out_oob) <- settings_calib$sitenames
  
  ## add evaluation result of all predicted data pooled
  extract_ddf_bysite <- function(site, out_oob){
    if (identical(NA, out_oob[[site]])){
      ddf <- NA
    } else {
      ddf <- out_oob[[site]]$gpp$fluxnet2015$data$ddf %>% 
        dplyr::select(date, gpp = mod)
    }
    return(ddf)
  }
  mod <- list()
  mod$daily <- purrr::map(
    as.list(settings_calib$sitenames),
    ~extract_ddf_bysite(., out_oob)
    )
  names(mod$daily) <- settings_calib$sitenames
  
  ## drop NAs from list
  na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
  mod$daily <- na.omit.list(mod$daily)
  
  mod$daily <- mod$daily %>% dplyr::bind_rows(.id = "sitename")
  out_oob$AALL <- eval_sofun( mod, settings_eval, settings_sims, obs_eval = ddf_obs_eval, overwrite = TRUE, light = TRUE )
  
  return(out_oob)
}

oob_calib_eval_sofun_bysite <- function(evalsite, setup, settings_calib, settings_eval, settings_sims, 
                                        settings_input, ddf_obs_calib, ddf_obs_eval ){
  
  print(paste("oob_calib_eval_sofun_bysite() for site", evalsite))
  
  outfil <- paste0(settings_calib$dir_results, "/out_eval_leftout_", evalsite, ".Rdata")
  
  if (file.exists(outfil)){

    print("loading file...")    
    load(outfil)

  } else {
    
    print("calibrating with left-out site...")
    
    ##------------------------------------------------
    ## Adjust calibration settings
    ##------------------------------------------------
    settings_calib$name = paste0("leftout_", evalsite)
    settings_calib$sitenames = settings_calib$sitenames[-which(settings_calib$sitenames == evalsite)]
    
    ##------------------------------------------------
    ## Get data for evaluation
    ##------------------------------------------------
    breaks_xdf <- ddf_obs_eval$breaks_xdf
    extract_obs_evalsite <- function(df, evalsite){
      df <- df %>% 
        dplyr::filter(sitename == evalsite)
      return(df)
    }
    ddf_obs_evalsite <- purrr::map(
      ddf_obs_eval[c("ddf", "xdf", "mdf", "adf")],
      ~extract_obs_evalsite(., evalsite)
    )
    ddf_obs_evalsite$breaks_xdf <- breaks_xdf
    
    ##------------------------------------------------
    ## Get data for calibration
    ##------------------------------------------------
    ddf_obs_calibsites <- ddf_obs_calib %>% 
      dplyr::filter(sitename != evalsite)
    
    ##------------------------------------------------
    ## Calibrate on left-out sites
    ##------------------------------------------------
    set.seed(1982)
    settings_calib <- calib_sofun(
      setup          = setup,
      settings_calib = settings_calib,
      settings_sims  = settings_sims,
      settings_input = settings_input,
      ddf_obs        = ddf_obs_calibsites
    )
    
    settings_eval$sitenames <- evalsite
    
    ##------------------------------------------------
    ## Update parameters and run at evaluation site
    ##------------------------------------------------
    filn <- paste0( settings_calib$dir_results, "/params_opt_", settings_calib$name, ".csv")
    params_opt <- readr::read_csv( filn )
    nothing <- update_params( params_opt, settings_sims$dir_sofun )
    
    settings_sims$sitenames <- evalsite
    mod <- runread_sofun( 
      settings = settings_sims, 
      setup = setup
    )
    
    ##------------------------------------------------
    ## Get evaluation results
    ##------------------------------------------------  
    out_eval <- try( eval_sofun( mod, settings_eval, settings_sims, obs_eval = ddf_obs_evalsite, overwrite = TRUE, light = TRUE ) )
    if (class(out_eval) == "try-error"){
      out_eval <- NA
    }
    
    # out <- out_eval$gpp$fluxnet2015$data$xdf %>%
    #   rbeni::analyse_modobs2(mod = "mod", obs = "obs", type = "heat")
    # out$gg
    
    ## write to file
    save(out_eval, file = outfil)
    
  }
  
  return(out_eval)
}
