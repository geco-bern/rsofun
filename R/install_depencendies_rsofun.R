#' Installs rsofun dependencies 
#'
#' Installs all dependencies of the rsofun package. This includes the following:
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
#'    \item \code{GenSA}
#' }
#' If argument `suggested = TRUE`, suggested dependencies are loaded too. These include:
#' \itemize{
#'   \item \code{BayesianTools}
#'   \item \code{caret}
#'   \item \code{gplots}
#'   \item \code{hydroGOF}
#'   \item \code{maps}
#'   \item \code{maptools}
#'   \item \code{neuralnet}
#'   \item \code{nnet}
#'   \item \code{raster}
#'   \item \code{sp}
#'   \item \code{testthat}
#' }  
#'
#' @param suggested A logical, defaults to `FALSE`. If set to `TRUE`, suggested depenendcies are installed too.
#' @return \code{NULL}
#' @export
#'
#' @examples install_dependencies_rsofun()
#' 
install_dependencies_rsofun <- function( suggested = FALSE ){

	dependencies <- c(  "dplyr", 
											"ggplot2", 
											"lubridate", 
											"Metrics", 
											"mgcv", 
											"ncdf4", 
											"optimr", 
											"purrr", 
											"readr", 
											"rlang", 
											"stringr", 
											"tidyr", 
											"GenSA", 
											"LSD"
											)
	new_packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
	if(length(new_packages)>0) install.packages(new_packages)

	if (suggested){
		dependencies <- c("BayesianTools", 
											"caret", 
											"gplots", 
											"hydroGOF", 
											"maps", 
											"maptools", 
											"neuralnet", 
											"nnet", 
											"raster", 
											"sp", 
											"testthat"
											) 
		new_packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
		if(length(new_packages)>0) install.packages(new_packages)
	}

}

