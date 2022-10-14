#' SOFUN p-model driver data
#'
#' Small tests dataset to validate if compiled code
#' and optimization routines can run
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{a character string containing the site name}
#'   \item{forcing}{a tibble of forcing climate data, including the following variables:
#'     \describe{
#'       \item{date}{Date of the observation in YYYY-MM-DD format}
#'       \item{temp}{Air temperature in $^\circ$C}
#'       \item{prec}{Precipitation in mm d$^{-1}$ (sum of rain and snow)}
#'       \item{vpd}{Vapour pressure deficit in Pa}
#'       \item{ppfd}{Photosynthetic photon flux density (PPFD) in mol m$^{-2}$ d$^{-1}$}
#'       \item{patm}{Atmospheric pressure in Pa}
#'       \item{ccov_int}{}
#'       \item{ccov}{Cloud coverage in %}
#'       \item{snow}{Snow in mm d$^{-1}$}
#'       \item{rain}{Rain in mm d$^{-1}$}
#'       \item{fapar}{Fraction of photosynthetic active radiation (fAPAR) measured in ...}
#'       \item{co2}{}
#'       \item{doy}{}
#'       \item{tmin}{Daily minimum air temperature in $^\circ$C}
#'       \item{tmax}{Daily maximum air temperature in $^\circ$C}
#'       }
#'   }
#'   \item{params_siml}{A tibble [ 1 x 18] of model parameters
#'     \describe{
#'       \item{spinup}{a logical value indicating ...}
#'       \item{spinupyears}{a numeric value ...}
#'       \item{recycle}{a numeric value ...}
#'       \item{soilmstress}{a logical value indicating whether there is ...}
#'       \item{tempstress}{a logical value indicating whether ...}
#'       \item{calc_aet_fapar_vpd}{a logical value indicating ...}
#'       \item{in_ppfd}{a logical value indicating ...}
#'       \item{in_netrad}{a logical value ...}
#'       \item{outdt}{a numeric value ...}
#'       \item{ltre}{a logical value ...}
#'       \item{ltne}{a logical value ...}
#'       \item{ltrd}{a logical value ...}
#'       \item{ltnd}{a logical value ...}
#'       \item{lgr3}{a logical value ...}
#'       \item{lgn3}{a logical value ...}
#'       \item{lgr4}{a logical value ...}
#'       \item{firstyeartrend}{the year ...}
#'       \item{nyeartrend}{the number of years ...}
#'     }
#'   }
#'   \item{site_info}{A tibble [ 1 x 12 ] containing site information
#'     \describe{
#'       \item{lon}{a numeric value indicating the longitud of the site location}
#'       \item{lat}{a numeric value indicating the latitude of the site location}
#'       \item{elv}{a numeric value indicating the elevation of the site location, in meters (?)}
#'       \item{year_end}{the year ...}
#'       \item{classid}{a character string which contains "EBF" if ... }
#'       \item{c4}{a logical value indicating ...}
#'       \item{whc}{a numeric value ...}
#'       \item{koeppen_code}{a character string indicating the Koeppen-Geiger code ...}
#'       \item{igbp_land_use}{a character string indicating ...}
#'       \item{plant_functional_type}{a character string ...}
#'       \item{date_start}{a Date[1:1] value indicating ...}
#'       \item{date_end}{a Date[1:1] value indicating ...}
#'     }
#'   }
#'   \item{params_soil}{A tibble [ 2 x 5 ] containing soil texture data
#'     \describe{
#'       \item{layer}{a character string containing "top" if the data on that row 
#'       is about the top layer of soil, or "bottom" if it's about the bottom layer}
#'       \item{fsand}{the fraction of sand in the soil}
#'       \item{fclay}{the fraction of clay in the soil}
#'       \item{forg}{the fraction of organic matter in the soil}
#'       \item{fgravel}{the fraction of gravel in the soil}
#'     }
#'   }
#' }
#' 
#' @references Pastorello, G., Trotta, C., Canfora, E. et al. 
#' The FLUXNET2015 dataset and the ONEFlux processing pipeline for eddy covariance data. 
#' Sci Data 7, 225 (2020). https://doi.org/10.1038/s41597-020-0534-3
#' 
#' University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2021): 
#' CRU TS4.05: Climatic Research Unit (CRU) Time-Series (TS) version 4.05 of high-resolution 
#' gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2020). 
#' NERC EDS Centre for Environmental Data Analysis, date of citation. 
#' https://catalogue.ceda.ac.uk/uuid/c26a65020a5e4b80b20018f148556681
#' 
#' Weedon, G. P., G. Balsamo, N. Bellouin,S. Gomes, M. J. Best, and P. Viterbo(2014), 
#' The WFDEI meteorologicalforcing data set: WATCH Forcing Datamethodology applied 
#' to ERA-Interimreanalysis data,
#' Water Resour. Res.,50,7505–7514, doi:10.1002/2014WR015638.
#' 
#' Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate 
#' surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
"p_model_drivers"

#' SOFUN p-model GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{a character string containing the site name (e.g. "FR-Pue")}
#'   \item{data}{a tibble [ 2,920 x 3 ] with time series for the following variables:
#'     \describe{
#'       \item{date}{a Date vector with format YYYY-MM-DD}
#'       \item{gpp}{the Gross Primary Productivity (GPP) for each time stamp ... (in .units.)}
#'       \item{gpp_unc}{the uncertainty of the GPP ...}
#'     }
#'   }
#' }
"p_model_validation"

#' SOFUN lm3ppa driver data
#'
#' Small tests dataset to validate if compiled code
#' and optimization routines can run using the
#' p-model specifications
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{params_siml}{model parameters}
#'   \item{site_info}{site information}
#'   \item{soil_texture}{soil texture data}
#'   \item{forcing}{forcing data}
#' }
"lm3ppa_p_model_drivers"

#' SOFUN lm3ppa driver data
#'
#' Small tests dataset to validate if compiled code
#' and optimization routines can run using the
#' Leuning specifications
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{params_siml}{model parameters}
#'   \item{site_info}{site information}
#'   \item{soil_texture}{soil texture data}
#'   \item{forcing}{forcing data}
#' }
"lm3ppa_gs_leuning_drivers"

#' SOFUN LM3PPA GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"lm3ppa_validation"

#' SOFUN LM3PPA GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"lm3ppa_validation_2"


#' SOFUN LM3PPA GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"lm3ppa_p_model_output"


#' SOFUN LM3PPA GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"lm3ppa_gs_leuning_output"

