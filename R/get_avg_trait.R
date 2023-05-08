#' Calculate average trait prediction
#' 
#' Average leaf trait prediction over growing season calculation
#' 
#' @param output Object returned by \code{runread_pmodel_f()}, with GPP and leaf
#' trait P-model predictions.
#' @param traits A character vector indicating the variable names of the leaf traits
#'  (from the P-model output), for example 'vcmax25'.
#' 
#' @return A weighted average of the leaf trait, where days with higher predicted
#' GPP are upweighted. This is a proxy for getting the growing season average leaf
#' trait prediction. This average can be used for calibration against leaf trait
#' observations, which are usually measured during the growing season.
#' 
#' @export

get_avg_trait <- function(
  output,
  traits
){
  # predefine variables for CRAN check compliance
  
  # might need to deal with the negtaive gpp
  res <- output |>
    apply(1, FUN = function(x){      # calculate avg per site (rowwise)
      df <- x$data[, traits]
      
      
      # remove negative GPP ---------- NOT NECESSARY
      gpp_vec <- sapply(x$data$gpp, function(y) max(0, y))
      gpp_weight <- gpp_vec / sum(gpp_vec)
      
      # return single trait values per site
      c(apply(df, 2, FUN = function(z) sum(z*gpp_weight)))
    }) |>
    t() |> data.frame()           # traspose
  
  cbind(sitename = output$sitename, res)
}
