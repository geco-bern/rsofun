# rsofun P-model driver data

Small dataset representing the driver to run the P-model at the FR-Pue
site. It can also be used together with daily GPP flux time series data
from CH-LAE
([`p_model_validation`](https://geco-bern.github.io/rsofun/dev/reference/p_model_validation.md))
to optimize model parameters. To optimize model parameters to leaf
traits data use the datasets
[`p_model_drivers_vcmax25`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers_vcmax25.md)
and
[`p_model_validation_vcmax25`](https://geco-bern.github.io/rsofun/dev/reference/p_model_validation_vcmax25.md).

## Usage

``` r
p_model_drivers
```

## Format

A tibble of driver data:

- sitename:

  A character string containing the site name.

- forcing:

  A tibble of a time series of forcing climate data, including the
  following data:

  date

  :   Date of the observation in YYYY-MM-DD format.

  temp

  :   Daytime average air temperature in \\^\circ\\C.

  vpd

  :   Daytime average vapour pressure deficit in Pa.

  ppfd

  :   Photosynthetic photon flux density (PPFD) in mol m\\^{-2}\\
      s\\^{-1}\\. If all values are NA, it indicates that PPFD should be
      calculated by the SPLASH model and column ccov.

  netrad

  :   Net radiation in W m\\^{-2}\\. WARNING: This is currently ignored
      as a model forcing.

  patm

  :   Atmospheric pressure in Pa.

  snow

  :   Snow in water equivalents mm s\\^{-1}\\.

  rain

  :   Rain as precipitation in liquid form in mm s\\^{-1}\\.

  tmin

  :   Daily minimum air temperature in \\^\circ\\C.

  tmax

  :   Daily maximum air temperature in \\^\circ\\C.

  fapar

  :   Fraction of photosynthetic active radiation (fAPAR), taking values
      between 0 and 1.

  co2

  :   Atmospheric CO\\\_2\\ concentration in ppm.

  ccov

  :   Cloud coverage in %. This is only used when either PPFD or net
      radiation are not prescribed.

- params_siml:

  A tibble of simulation parameters, including the following data:

  spinup

  :   A logical value indicating whether this simulation does spin-up.

  spinupyears

  :   Number of spin-up years.

  recycle

  :   Number of first N years of forcing data.frame that are recycled
      for spin-up.

  outdt

  :   An integer indicating the output periodicity.

  ltre

  :   A logical value, `TRUE` if evergreen tree.

  ltne

  :   A logical value, `TRUE` if evergreen tree and N-fixing.

  ltrd

  :   A logical value, `TRUE` if deciduous tree.

  ltnd

  :   A logical value, `TRUE` if deciduous tree and N-fixing.

  lgr3

  :   A logical value, `TRUE` if grass with C3 photosynthetic pathway.

  lgn3

  :   A logical value, `TRUE` if grass with C3 photosynthetic pathway
      and N-fixing.

  lgr4

  :   A logical value, `TRUE` if grass with C4 photosynthetic pathway.

- site_info:

  A tibble containing site meta information. This data structure can be
  freely used for documenting the dataset, but must include at least the
  following data:

  lon

  :   Longitude of the site location in degrees east.

  lat

  :   Latitude of the site location in degrees north.

  elv

  :   Elevation of the site location, in meters above sea level.

  whc

  :   A numeric value for the rooting zone water holding capacity (in
      mm)

## Source

Pastorello, G., Trotta, C., Canfora, E. et al. The FLUXNET2015 dataset
and the ONEFlux processing pipeline for eddy covariance data. Sci Data
7, 225 (2020). https://doi.org/10.1038/s41597-020-0534-3

University of East Anglia Climatic Research Unit; Harris, I.C.; Jones,
P.D.; Osborn, T. (2021): CRU TS4.05: Climatic Research Unit (CRU)
Time-Series (TS) version 4.05 of high-resolution gridded data of
month-by-month variation in climate (Jan. 1901- Dec. 2020). NERC EDS
Centre for Environmental Data Analysis, date of citation.
https://catalogue.ceda.ac.uk/uuid/c26a65020a5e4b80b20018f148556681

Weedon, G. P., G. Balsamo, N. Bellouin,S. Gomes, M. J. Best, and P.
Viterbo(2014), The WFDEI meteorologicalforcing data set: WATCH Forcing
Datamethodology applied to ERA-Interimreanalysis data, Water Resour.
Res.,50,7505–7514, doi:10.1002/2014WR015638.

Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology 37 (12): 4302-4315.
