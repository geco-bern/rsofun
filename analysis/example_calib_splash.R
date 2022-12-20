
# new water stress function for supply rate-------------------------------------
calc_waterstress <- function(wcont){
  k_rzwsc = 1/10
  pwp = 10
  pmax(1.0 - exp(-k_rzwsc * (wcont - pwp)), 0)
}

ggplot() +
  geom_function(fun = calc_waterstress) + 
  xlim(0, 240) +
  ylim(0, 1) +
  geom_hline(yintercept = 0, linetype = "dotted")

# collect soil parameters from SoilGrids----------------------------------------
settings_soilgrids <- get_settings_soilgrids(varnam = c("cfvo", "clay", "sand", "soc"), layer = 1:6)

df_soilgrids <- ingest(
  siteinfo_fluxnet2015 |> 
    select(sitename, lon, lat) |> 
    slice(3),
  source    = "soilgrids",
  settings  = settings_soilgrids
)

# make df_soilgrids into the right format for p_model_drivers$params_soil[[1]]

