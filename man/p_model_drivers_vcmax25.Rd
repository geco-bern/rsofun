% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{p_model_drivers_vcmax25}
\alias{p_model_drivers_vcmax25}
\title{rsofun p-model driver data (for leaf traits)}
\format{
A tibble of model driver data:
\describe{
  \item{sitename}{A character string containing the site names.}
  \item{forcing}{A tibble of forcing climate data for an "average" year, 
  including the following variables:
    \describe{
      \item{date}{Date in YYYY-MM-DD format (here representative of the day of the year instead
      of the date of observation).}
      \item{temp}{Air temperature in \eqn{^\circ}C.}
      \item{vpd}{Vapour pressure deficit in Pa.}
      \item{ppfd}{Photosynthetic photon flux density (PPFD) in 
      mol m\eqn{^{-2}} s\eqn{^{-1}}. If all values are NA, it indicates that
      PPFD should be calculated by the SPLASH model.}
      \item{netrad}{Net radiation in W m\eqn{^{-2}}. This is currently 
      ignored as a model forcing.}
      \item{patm}{Atmospheric pressure in Pa.}
      \item{ccov}{Cloud coverage in \%. This is only used when either PPFD or
      net radiation are not prescribed.}
      \item{snow}{Snow in mm d\eqn{^{-1}}.}
      \item{rain}{Rain in mm d\eqn{^{-1}}.}
      \item{fapar}{Fraction of photosynthetic active radiation (fAPAR), taking
     values between 0 and 1.}
      \item{co2}{Annually varying observed atmospheric CO\eqn{_2}, identical 
      across sites.}
      \item{tmin}{Daily minimum air temperature in \eqn{^\circ}C (set equal to temp).}
      \item{tmax}{Daily maximum air temperature in \eqn{^\circ}C.(set equal to temp).}
      }
  }
  \item{params_siml}{A tibble containing simulation parameters.
    \describe{
      \item{spinup}{A logical value indicating whether this simulation does spin-up.}
      \item{spinupyears}{Number of spin-up years.}
      \item{recycle}{Length of standard recycling period, in days.}
      \item{outdt}{An integer indicating the output periodicity.}
      \item{ltre}{A logical value, \code{TRUE} if evergreen tree.}
      \item{ltne}{A logical value, \code{TRUE} if evergreen tree and N-fixing.}
      \item{ltrd}{A logical value, \code{TRUE} if deciduous tree.}
      \item{ltnd}{A logical value, \code{TRUE} if deciduous tree and N-fixing.}
      \item{lgr3}{A logical value, \code{TRUE} if grass with C3 photosynthetic pathway.}
      \item{lgn3}{A logical value, \code{TRUE} if grass with C3 photosynthetic
      pathway and N-fixing.}
      \item{lgr4}{A logical value, \code{TRUE} if grass with C4 photosynthetic pathway.}
    }
  }
  \item{site_info}{A tibble containing site meta information.
    \describe{
      \item{lon}{Longitud of the site location.}
      \item{lat}{Latitude of the site location.}
      \item{elv}{Elevation of the site location, in meters.}
      \item{whc}{A numeric value for the root zone water holding capacity (in mm), used for 
      simulating the soil water balance.}
    }
  }
}
}
\source{
Atkin, O. K., Bloomfield, K. J., Reich, P. B., Tjoelker, M. G., Asner, G. P., Bonal, D., et al. (2015). 
Global variability in leaf respiration in relation to climate, plant functional types and leaf traits. 
New Phytol. 206 (2), 614–636. doi:10.1111/nph.13253

University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2021): 
CRU TS4.05: Climatic Research Unit (CRU) Time-Series (TS) version 4.05 of high-resolution 
gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2020). 
NERC EDS Centre for Environmental Data Analysis, date of citation. 
https://catalogue.ceda.ac.uk/uuid/c26a65020a5e4b80b20018f148556681

Weedon, G. P., G. Balsamo, N. Bellouin,S. Gomes, M. J. Best, and P. Viterbo(2014), 
The WFDEI meteorologicalforcing data set: WATCH Forcing Datamethodology applied 
to ERA-Interimreanalysis data,
Water Resour. Res.,50,7505–7514, doi:10.1002/2014WR015638.

Fick, S.E. and R.J. Hijmans, 2017. 
WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. 
International Journal of Climatology 37 (12): 4302-4315.

C.D. Keeling, R.B. Bacastow, A.E. Bainbridge, C.A. Ekdahl, P.R. Guenther, and L.S. Waterman, (1976), 
Atmospheric carbon dioxide variations at Mauna Loa Observatory, Hawaii, Tellus, vol. 28, 538-551
}
\usage{
p_model_drivers_vcmax25
}
\description{
Small tests dataset to validate if compiled code
and optimization routines can run for leaf traits data
}
\keyword{datasets}
