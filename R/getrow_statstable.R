#' Summarise performance statistics.
#'
#' Extracts performance statistics from evaluation object and returns single-row dataframe with respective information.
#'
#' @param out_eval An object retruned from a function call to eval_sofun()
#' @param stat A character string specifying which statistic to be used (Use "rsq" for the adjusted R2, "rmse" for the root mean square error).
#'
#' @return A data frame (tibble) with one row and multiple columns for different scales at which statistics are evaluated (spatial, annual, monthly, daily, mean seasonal cycle, annual anomalies, daily anomalies)
#' @export
#'
#' @examples out_eval <- eval_sofun( mod, settings_eval, settings_sims_sitescale, siteinfo, obs_eval = obs_eval_NT, doplot=FALSE, overwrite = TRUE ); getrow_statstable( out_eval, "rsq" )
#'
getrow_statstable <- function( out_eval, stat ){
  addrow <- tibble( spatial         = out_eval$metrics$gpp$fluxnet2015$spatial[[ stat ]],
                    annual          = out_eval$metrics$gpp$fluxnet2015$annual_pooled[[ stat ]],
                    monthly         = out_eval$metrics$gpp$fluxnet2015$monthly_pooled[[ stat ]],
                    # xdaily          = out_eval$metrics$gpp$fluxnet2015$xdaily_pooled[[ stat ]],
                    daily           = out_eval$metrics$gpp$fluxnet2015$daily_pooled[[ stat ]],
                    seasonal        = out_eval$metrics$gpp$fluxnet2015$meandoy[[ stat ]],
                    # seasonal_xdaily = out_eval$metrics$gpp$fluxnet2015$meanxoy[[ stat ]],
                    annual_var      = out_eval$metrics$gpp$fluxnet2015$anomalies_annual[[ stat ]],
                    # xdaily_var      = out_eval$metrics$gpp$fluxnet2015$anomalies_xdaily[[ stat ]],
                    daily_var       = out_eval$metrics$gpp$fluxnet2015$anomalies_daily[[ stat ]]
                    )
  return(addrow)
}