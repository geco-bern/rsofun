#' rsofun P-model driver data
#'
#' Small tests dataset to validate if compiled code
#' and optimization routines can run
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{A character string containing the site name.}
#'   \item{forcing}{A tibble of a time series of forcing climate data, including 
#'   the following variables:
#'     \describe{
#'       \item{date}{Date of the observation in YYYY-MM-DD format.}
#'       \item{temp}{Daytime average air temperature in \eqn{^\circ}C.}
#'       \item{vpd}{Daytime average vapour pressure deficit in Pa.}
#'       \item{ppfd}{Photosynthetic photon flux density (PPFD) in 
#'       mol m\eqn{^{-2}} s\eqn{^{-1}}. If all values are NA, it indicates that
#'       PPFD should be calculated by the SPLASH model.}
#'       \item{netrad}{Net radiation in W m\eqn{^{-2}}. If all values are NA,
#'       it indicates that net radiation should be calculated by the SPLASH
#'       model.}
#'       \item{patm}{Atmospheric pressure in Pa.}
#'       \item{snow}{Snow in water equivalents mm s\eqn{^{-1}}.}
#'       \item{rain}{Rain as precipitation in liquid form in mm s\eqn{^{-1}}.}
#'       \item{tmin}{Daily minimum air temperature in \eqn{^\circ}C.}
#'       \item{tmax}{Daily maximum air temperature in \eqn{^\circ}C.}
#'       \item{fapar}{Fraction of photosynthetic active radiation (fAPAR), taking
#'      values between 0 and 1.}
#'       \item{co2}{Atmospheric CO\eqn{_2} concentration.}
#'       \item{ccov}{Cloud coverage in \%. This is only used when either PPFD or
#'       net radiation are not prescribed.}
#'       }
#'   }
#'   \item{params_siml}{A tibble of simulation parameters.
#'     \describe{
#'       \item{spinup}{A logical value indicating whether this simulation does spin-up.}
#'       \item{spinupyears}{Number of spin-up years.}
#'       \item{recycle}{Length of standard recycling period, in days.}
#'       \item{outdt}{An integer indicating the output periodicity.}
#'       \item{ltre}{A logical value, \code{TRUE} if evergreen tree.}
#'       \item{ltne}{A logical value, \code{TRUE} if evergreen tree and N-fixing.}
#'       \item{ltrd}{A logical value, \code{TRUE} if deciduous tree.}
#'       \item{ltnd}{A logical value, \code{TRUE} if deciduous tree and N-fixing.}
#'       \item{lgr3}{A logical value, \code{TRUE} if grass with C3 photosynthetic pathway.}
#'       \item{lgn3}{A logical value, \code{TRUE} if grass with C3 photosynthetic
#'       pathway and N-fixing.}
#'       \item{lgr4}{A logical value, \code{TRUE} if grass with C4 photosynthetic pathway.}
#'     }
#'   }
#'   \item{site_info}{A tibble containing site meta information.
#'     \describe{
#'       \item{lon}{Longitude of the site location in degrees east.}
#'       \item{lat}{Latitude of the site location in degrees north.}
#'       \item{elv}{Elevation of the site location, in meters above sea level.}
#'       \item{whc}{A numeric value for the rooting zone water holding capacity (in mm)}
#'     }
#'   }
#' }
#' 
#' @source Pastorello, G., Trotta, C., Canfora, E. et al. 
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
#' calibration routines for a time series of fluxes.
#'
#' @format A tibble of validation data:
#' \describe{
#'   \item{sitename}{A character string containing the site name (e.g. 'FR-Pue').}
#'   \item{data}{A tibble [ 2,920 x 3 ] with time series for the following variables:
#'     \describe{
#'       \item{date}{Date vector with format YYYY-MM-DD.}
#'       \item{gpp}{The observed Gross Primary Productivity (GPP) for each time stamp 
#'       (in gC m\eqn{^{-2}} d\eqn{^{-1}}).}
#'       \item{gpp_unc}{The uncertainty of the GPP (in gC m\eqn{^{-2}} d\eqn{^{-1}}).}
#'     }
#'   }
#' }
#' @examples require(ggplot2); require(tidyr)
#' p_model_validation %>% tidyr::unnest(data) 
#' 
#' @source Pastorello, G., Trotta, C., Canfora, E. et al. 
#' The FLUXNET2015 dataset and the ONEFlux processing pipeline for eddy covariance data. 
#' Sci Data 7, 225 (2020). https://doi.org/10.1038/s41597-020-0534-3
"p_model_validation"

#' rsofun p-model driver data (for leaf traits)
#'
#' Small tests dataset to validate if compiled code
#' and optimization routines can run for leaf traits data
#'
#' @format A tibble of model driver data:
#' \describe{
#'   \item{sitename}{A character string containing the site names.}
#'   \item{forcing}{A tibble of forcing climate data for an "average" year, 
#'   including the following variables:
#'     \describe{
#'       \item{date}{Date in YYYY-MM-DD format (here representative of the day of the year instead
#'       of the date of observation).}
#'       \item{temp}{Air temperature in \eqn{^\circ}C.}
#'       \item{vpd}{Vapour pressure deficit in Pa.}
#'       \item{ppfd}{Photosynthetic photon flux density (PPFD) in 
#'       mol m\eqn{^{-2}} s\eqn{^{-1}}. If all values are NA, it indicates that
#'       PPFD should be calculated by the SPLASH model.}
#'       \item{netrad}{Net radiation in W m\eqn{^{-2}}. If all values are NA,
#'       it indicates that net radiation should be calculated by the SPLASH
#'       model.}
#'       \item{patm}{Atmospheric pressure in Pa.}
#'       \item{ccov}{Cloud coverage in \%. This is only used when either PPFD or
#'       net radiation are not prescribed.}
#'       \item{snow}{Snow in mm d\eqn{^{-1}}.}
#'       \item{rain}{Rain in mm d\eqn{^{-1}}.}
#'       \item{fapar}{Fraction of photosynthetic active radiation (fAPAR), taking
#'      values between 0 and 1.}
#'       \item{co2}{Annually varying observed atmospheric CO\eqn{_2}, identical 
#'       across sites.}
#'       \item{tmin}{Daily minimum air temperature in \eqn{^\circ}C (set equal to temp).}
#'       \item{tmax}{Daily maximum air temperature in \eqn{^\circ}C.(set equal to temp).}
#'       }
#'   }
#'   \item{params_siml}{A tibble containing simulation parameters.
#'     \describe{
#'       \item{spinup}{A logical value indicating whether this simulation does spin-up.}
#'       \item{spinupyears}{Number of spin-up years.}
#'       \item{recycle}{Length of standard recycling period, in days.}
#'       \item{outdt}{An integer indicating the output periodicity.}
#'       \item{ltre}{A logical value, \code{TRUE} if evergreen tree.}
#'       \item{ltne}{A logical value, \code{TRUE} if evergreen tree and N-fixing.}
#'       \item{ltrd}{A logical value, \code{TRUE} if deciduous tree.}
#'       \item{ltnd}{A logical value, \code{TRUE} if deciduous tree and N-fixing.}
#'       \item{lgr3}{A logical value, \code{TRUE} if grass with C3 photosynthetic pathway.}
#'       \item{lgn3}{A logical value, \code{TRUE} if grass with C3 photosynthetic
#'       pathway and N-fixing.}
#'       \item{lgr4}{A logical value, \code{TRUE} if grass with C4 photosynthetic pathway.}
#'     }
#'   }
#'   \item{site_info}{A tibble containing site meta information.
#'     \describe{
#'       \item{lon}{Longitud of the site location.}
#'       \item{lat}{Latitude of the site location.}
#'       \item{elv}{Elevation of the site location, in meters.}
#'       \item{whc}{A numeric value for the root zone water holding capacity (in mm), used for 
#'       simulating the soil water balance.}
#'     }
#'   }
#' }
#' 
#' @source Atkin, O. K., Bloomfield, K. J., Reich, P. B., Tjoelker, M. G., Asner, G. P., Bonal, D., et al. (2015). 
#' Global variability in leaf respiration in relation to climate, plant functional types and leaf traits. 
#' New Phytol. 206 (2), 614–636. doi:10.1111/nph.13253
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
#' Fick, S.E. and R.J. Hijmans, 2017. 
#' WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. 
#' International Journal of Climatology 37 (12): 4302-4315.
#' 
#' C.D. Keeling, R.B. Bacastow, A.E. Bainbridge, C.A. Ekdahl, P.R. Guenther, and L.S. Waterman, (1976), 
#' Atmospheric carbon dioxide variations at Mauna Loa Observatory, Hawaii, Tellus, vol. 28, 538-551
"p_model_drivers_vcmax25"

#' SOFUN p-model Vcmax25 validation data
#'
#' Small tests dataset to validate 
#' calibration routines for leaf traits.
#'
#' @format A tibble of validation data:
#' \describe{
#'   \item{sitename}{A character string containing the site names (e.g. 'Reichetal_Colorado').}
#'   \item{data}{A tibble [ 1 x 2 ] with observations for the following variables:
#'     \describe{
#'       \item{vcmax25}{The observed maximum rate of carboxylation (Vcmax), normalised 
#'     to 25\eqn{^o} C (in mol C m\eqn{^{-2}} d\eqn{^{-1}}), aggregated over different plant species
#'     in each site.}
#'       \item{vcmax25_unc}{The uncertainty of the Vcmax25 (in mol C m\eqn{^{-2}} d\eqn{^{-1}}),
#'       calculated as the standard deviation among Vcmax25 observations for
#'       several species per site or as the total standard deviation across sites for
#'       single-plant-species sites.}
#'     }
#'   }
#' }
#' @examples require(ggplot2); require(tidyr)
#' p_model_validation_vcmax25 %>% tidyr::unnest(data) 
#' 
#' @source Atkin, O. K., Bloomfield, K. J., Reich, P. B., Tjoelker, M. G., Asner, G. P., Bonal, D., et al. (2015). 
#' Global variability in leaf respiration in relation to climate, plant functional types and leaf traits. 
#' New Phytol. 206 (2), 614–636. doi:10.1111/nph.13253
"p_model_validation_vcmax25"

#' rsofun BiomeE driver data
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
"biomee_p_model_drivers"

#' rsofun BiomeE driver data
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
"biomee_gs_leuning_drivers"

#' rsofun BiomeE GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"biomee_validation"

#' rsofun BiomeE GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"biomee_validation_2"


#' rsofun BiomeE GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"biomee_p_model_output"


#' rsofun BiomeE GPP validation data
#'
#' Small tests dataset to validate 
#' calibration routines
#'
#' @format A tibble of driver data:
#' \describe{
#'   \item{sitename}{site name}
#'   \item{data}{validation dta}
#' }
"biomee_gs_leuning_output"

