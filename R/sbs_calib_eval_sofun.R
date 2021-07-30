#' Conducts an site-by-site calibration
#'
#' Wraps around calib_sofun() to calibrate site-by-site (with one single site data for calibration and evaluation) and returns the evaluation result.
#' 
#' @param settings_calib A list containing model calibration settings. See vignette_rsofun.pdf for more information and examples.
#' @param settings_eval A list specifying evaluation settings 
#' (see vignette eval_sofun.pdf for more information and examples)
#' @param settings_sims A list containing model simulation settings from \code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more information and examples.
#' @param settings_input A list containing model input settings. See vignette_rsofun.pdf for more information and examples.
#' @param df_drivers xxx
#' @param ddf_obs_calib A data frame containing observational data used for model calibration. Created by function \code{get_obs_calib()}
#' @param obs_eval A data frame containing observational data used for model evaluation Created by function \code{get_obs_eval()}
#' @param overwrite A boolean, defaults to \code{FALSE}.
#'
#' @return A nested list of objects returned by \code{\link{eval_sofun}}.
#' @export
#'
#' @examples xxx
#' 
sbs_calib_eval_sofun <- function( settings_calib, settings_eval, settings_sims, settings_input, df_drivers, ddf_obs_calib, obs_eval,
                                         overwrite){

  ## Get list of results from out-of-bag calibration 
  out_sbs <- purrr::map(
    as.list(settings_calib$sitenames),
    ~sbs_calib_eval_sofun_bysite(., 
                                 settings_calib, 
                                 settings_eval, 
                                 settings_sims, 
                                 settings_input, 
                                 df_drivers = df_drivers,
                                 ddf_obs_calib = ddf_obs_calib, 
                                 obs_eval = obs_eval,
                                 overwrite = overwrite
                                 )
                                )
  
  names(out_sbs) <- settings_calib$sitenames
  
  ## add evaluation result of all predicted data pooled
  extract_ddf_bysite <- function(site, out_sbs){
    if (identical(NA, out_sbs[[site]])){
      ddf <- NA
    } else {
      ddf <- out_sbs[[site]][[settings_calib$targetvars]]$fluxnet2015$data$ddf %>% 
        dplyr::select(sitename, date, mod) %>% 
        dplyr::rename( !!settings_calib$targetvars := mod)
    }
    return(ddf)
  }
  mod <- purrr::map(
    as.list(settings_calib$sitenames),
    ~extract_ddf_bysite(., out_sbs)
    ) %>% 
    bind_rows()

  # out_sbs$AALL <- eval_sofun( mod, settings_eval, settings_sims, obs_eval = obs_eval, overwrite = TRUE, light = TRUE )
  
  return(out_sbs)
}



sbs_calib_eval_sofun_bysite <- function(evalsite, settings_calib, settings_eval, settings_sims, 
                                        settings_input, df_drivers, ddf_obs_calib, obs_eval, overwrite ){
  
  print(paste("sbs_calib_eval_sofun_bysite() for site", evalsite))
  
  dirn <- paste0( settings_calib$dir_results, "/sbs_", settings_calib$name)

  if (!dir.exists(dirn)){
    system(paste0("mkdir -p ", dirn))
  }
  outfil <- paste0(dirn, "/out_eval_leftout_", evalsite, ".Rdata")

  
  if (file.exists(outfil) && !overwrite){

    print("loading file...")    
    load(outfil)

  } else {
    
    print("calibrating with left-out site...")
    
    ##------------------------------------------------
    ## Adjust calibration settings
    ##------------------------------------------------
    settings_calib$name = paste0("leftout_", evalsite)
    settings_calib$sitenames = evalsite

    ## overwrite settings to write site-level calibration results to sub-directory
    settings_calib$dir_results <- dirn

    ##------------------------------------------------
    ## Get data for evaluation
    ##------------------------------------------------
    breaks_xdf <- obs_eval$breaks_xdf
    extract_obs_evalsite <- function(df, evalsite){
      df <- df %>% 
        dplyr::filter(sitename == evalsite)
      return(df)
    }
    obs_evalsite <- purrr::map(
      obs_eval[c("ddf", "xdf", "mdf", "adf")],
      ~extract_obs_evalsite(., evalsite)
    )
    obs_evalsite$breaks_xdf <- breaks_xdf
    
    ##------------------------------------------------
    ## Get data for calibration
    ##------------------------------------------------
    ddf_obs_calibsites <- ddf_obs_calib %>% 
      dplyr::filter(sitename == evalsite)
    
    ##------------------------------------------------
    ## Calibrate on left-out sites
    ##------------------------------------------------
    set.seed(1982)
    settings_calib <- calib_sofun( 
      settings_calib, 
      dplyr::filter(df_drivers, sitename == evalsite), 
      ddf_obs = ddf_obs_calibsites 
      )
    
    settings_eval$sitenames <- evalsite
    
    ##------------------------------------------------
    ## Update parameters and run at evaluation site
    ##------------------------------------------------
    params_modl <- list(
      kphio           = 0.04997714009213085,
      soilm_par_a     = 1.0,
      soilm_par_b     = 0.0
      )
    
    # settings_sims$sitenames <- evalsite
    mod <- runread_sofun_f(
      dplyr::filter(df_drivers, sitename == evalsite), 
      params_modl = params_modl, 
      makecheck = TRUE,
      parallel = FALSE
      ) %>% 
      rename(id = sitename) %>% 
      unnest(out_sofun)
    
    ##------------------------------------------------
    ## Get evaluation results
    ##------------------------------------------------  
    out_eval <- try( 
      eval_sofun( 
        mod, 
        settings_eval, 
        settings_sims, 
        obs_eval = obs_evalsite, 
        overwrite = TRUE, 
        light = TRUE 
        ) 
      )
    if (class(out_eval) == "try-error"){
      out_eval <- NA
    }
    
    ## write to file
    save(out_eval, file = outfil)
    
  }
  
  return(out_eval)
}
