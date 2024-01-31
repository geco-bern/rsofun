read_meta_fdk = function (site, path, meta_data = FALSE, 
          out_path) 
{
  IGBP_veg_long <- time <- TIMESTAMP_START <- TIMESTAMP_END <- P <- TA_F <- PA_F <- CO2_F <- P_F <- TA_F_MDS <- CO2_F_MDS <- NULL
  files <- list.files(path, utils::glob2rx(paste0(site, "*.nc")), 
                      full.names = TRUE, recursive = TRUE)
  if (length(files) != 2) {
    stop("Missing either flux or meteo data for the requested site")
  }
  if (meta_data) {
    files <- files[grepl(utils::glob2rx("*_Flux.nc"), files)]
  }
  df <- lapply(files, function(file) {
    nc <- ncdf4::nc_open(file)
    time <- ncdf4::ncvar_get(nc, "time")
    time_units <- strsplit(ncdf4::ncatt_get(nc, "time")$units, 
                           "seconds since ")[[1]][2]
    time_date <- lubridate::ymd_hms(time_units, tz = "GMT") + 
      lubridate::seconds(time)
    vars <- names(nc$var)
    df <- as.data.frame(lapply(vars, function(x) ncdf4::ncvar_get(nc, 
                                                                  x)))
    ncdf4::nc_close(nc)
    colnames(df) <- vars
    df$time <- time_date
    if ("IGBP_veg_short" %in% colnames(df)) {
      df$IGBP_veg_short <- trimws(df$IGBP_veg_short)
      
      df$IGBP_veg_short = case_match(df$IGBP_veg_short,
                                     "Woody Savannas" ~ "WSA",
                                     "Savannas" ~ "SAV",
                                     "Permanent Wetlands" ~ "WET",
                                     "Open Shrublands" ~ "OSH",
                                     "Closed Shrublands" ~ "CSH",
                                     "Grasslands" ~ "GRA",
                                     "Evergreen Needleleaf Forest" ~ "ENF",
                                     "Evergreen Broadleaf Forest" ~ "EBF",
                                     "Deciduous Broadleaf Forest" ~ "DBF",
                                     "Mixed Forest" ~ "MF",
                                     "Croplands" ~ "CRO",
                                     "Cropland/Natural Vegetation Mosaic" ~ "MF",
                                     "Urban and Built-Up" ~ NA,
                                     .default = df$IGBP_veg_short)
    }
    else {
      warning("Column 'IGBP_veg_short' does not exist in the data frame. Assigning NA.")
      df$IGBP_veg_short <- NA
    }
    if ("IGBP_veg_long" %in% names(df)) {
      df <- subset(df, select = -IGBP_veg_long)
    }
    if (meta_data) {
      meta_columns <- c("latitude", "longitude", "reference_height", 
                        "canopy_height", "elevation", "IGBP_veg_short", 
                        "year_start", "year_end")
      missing_columns <- setdiff(meta_columns, colnames(df))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          warning(paste("Column", col, "does not exist in the data frame. Assigning NA."))
          df[, col] <- NA
        }
      }
      df$year_start <- format(min(df$time), "%Y")
      df$year_end <- format(max(df$time), "%Y")
      df <- df[1, meta_columns]
      df$sitename <- site
    }
    return(df)
  })
  if (meta_data) {
    return(df)
  }
  else {
    all <- suppressMessages(dplyr::left_join(df[[1]], df[[2]], 
                                             by = "time"))
  }

}
