library(xtable)
library(readr, quietly = TRUE)
library(dplyr, quietly = TRUE)

caption <- "Sites used for evaluation. Lon. is longitude, negative values indicate west longitude; Lat. is latitude, positive values indicate north latitude; Veg. is vegetation type: deciduous broadleaf forest (DBF); evergreen broadleaf forest (EBF); evergreen needleleaf forest (ENF); grassland (GRA); mixed deciduous and evergreen needleleaf forest (MF); savanna ecosystem (SAV); shrub ecosystem (SHR); wetland (WET)."

siteinfo <- read_csv("siteinfo_eval.csv") %>%
            mutate( Reference = paste0("\cite{", sitename ,"}") ) %>%
            mutate( Period = paste0(as.character(year_start), "-", as.character(year_end)) ) %>% 
            dplyr::select( -year_start, -year_end ) %>%
						dplyr::rename( Site=sitename, Lon.=lon, Lat.=lat, Elevation=elv, Veg.=classid, Clim.=koeppen_code, N = ndailygpp ) %>%
						dplyr::select( Site, Lon., Lat., Period, Veg., Clim., N, Reference )

latextable <- xtable( siteinfo, caption = caption )

print( latextable, hline.after=c(-1, 0), tabular.environment = "longtable")