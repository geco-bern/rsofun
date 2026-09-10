#!/usr/bin/env Rscript

# Generate quality-controlled MODIS LAI observations for FLUXNET site CH-Oe1.
#
# Product: MOD15A2H (Terra, 8-day, 500 m, Collection 6.1)
# Site: CH-Oe1, 47.28583 degrees N, 7.731944 degrees E
# Period: 2002-01-01 through 2008-12-31
# QC definitions:
# https://modis-land.gsfc.nasa.gov/pdf/MOD15_C61_UserGuide_April2020.pdf

library(dplyr)
library(MODISTools)
library(tidyr)

modis_raw <- MODISTools::mt_subset(
  product = "MOD15A2H",
  band = c("Lai_500m", "FparLai_QC"),
  site_id = "CH-Oe1",
  network = "FLUXNET",
  start = "2002-01-01",
  end = "2008-12-31",
  internal = TRUE,
  progress = TRUE
)

modis_lai_ch_oe1 <- modis_raw |>
  mutate(
    center_pixel = (as.integer(nrows) * as.integer(ncols) + 1L) %/% 2L
  ) |>
  filter(pixel == center_pixel) |>
  transmute(
    date = as.Date(calendar_date),
    pixel,
    band,
    value = as.integer(value)
  ) |>
  pivot_wider(names_from = band, values_from = value) |>
  mutate(
    modland_qc = bitwAnd(FparLai_QC, 1L),
    dead_detector = bitwAnd(bitwShiftR(FparLai_QC, 2L), 1L),
    cloud_state = bitwAnd(bitwShiftR(FparLai_QC, 3L), 3L),
    scf_qc = bitwAnd(bitwShiftR(FparLai_QC, 5L), 7L)
  ) |>
  filter(
    between(Lai_500m, 0L, 100L),
    modland_qc == 0L,
    dead_detector == 0L,
    cloud_state == 0L,
    scf_qc %in% 0:1
  ) |>
  transmute(
    date,
    lai = 0.1 * Lai_500m,
    fpar_lai_qc = FparLai_QC,
    modland_qc,
    dead_detector,
    cloud_state,
    scf_qc
  )

usethis::use_data(modis_lai_ch_oe1, overwrite = TRUE, compress = "xz")
