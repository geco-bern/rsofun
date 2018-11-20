#' Loads rsofun dependencies 
#'
#' Loads all dependencies of the rsofun package with a \code{require} call. This includes the following:
#' \itemize{
#'    \item \code{dplyr}
#'    \item \code{ggplot2}
#'    \item \code{lubridate}
#'    \item \code{Metrics}
#'    \item \code{mgcv}
#'    \item \code{ncdf4}
#'    \item \code{optimr}
#'    \item \code{purrr}
#'    \item \code{readr}
#'    \item \code{rlang}
#'    \item \code{stringr}
#'    \item \code{tidyr}
#'    \item \code{LSD}
#' }  
#'
#' @return \code{NULL}
#' @export
#'
#' @examples NULL <- load_dependencies_rsofun()
#' 
load_dependencies_rsofun <- function(){

	require(dplyr)
	require(ggplot2)
	require(lubridate)
	require(Metrics)
	require(mgcv)
	require(ncdf4)
	require(optimr)
	require(purrr)
	require(readr)
	require(rlang)
	require(stringr)
	require(tidyr)
	require(LSD)

	return(NULL)
}

#' Loads suggested rsofun dependencies 
#'
#' Loads all suggested dependencies of the rsofun package with a \code{require} call. This includes the following:
#' \itemize{
#'   \item \code{BayesianTools}
#'   \item \code{caret}
#'   \item \code{GenSA}
#'   \item \code{gplots}
#'   \item \code{hydroGOF}
#'   \item \code{maps}
#'   \item \code{maptools}
#'   \item \code{neuralnet}
#'   \item \code{nnet}
#'   \item \code{raster}
#'   \item \code{sp}
#'   \item \code{testtha}
#' }  
#'
#' @return \code{NULL}
#' @export
#'
#' @examples NULL <- load_dependencies_suggest_rsofun()
#' 
load_dependencies_suggest_rsofun <- function(){

	require(BayesianTools)
	require(caret)
	require(GenSA)
	require(gplots)
	require(hydroGOF)
	require(maps)
	require(maptools)
	require(neuralnet)
	require(nnet)
	require(raster)
	require(sp)
	require(testtha)

	return(NULL)

}
