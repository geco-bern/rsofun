The script for generating `data/p_model_drivers.rda` is in [`geco-bern/FluxDataKit/analysis/05_generate_demo_rsofun_drivers.R`](https://github.com/geco-bern/FluxDataKit/blob/main/analysis/05_generate_demo_rsofun_drivers.R).

2025-02: added vwind=2.0 m/s with following code:
```
load("~/GitHub/geco-bern/rsofun/data/p_hydro_drivers.rda")
p_hydro_drivers <- p_hydro_drivers |> rowwise() |> 
  mutate(forcing = list(mutate(forcing, vwind=2.0)), 
         forcing_daytime_mean = list(mutate(forcing_daytime_mean, vwind=2.0)), 
         forcing_halfhourly = list(mutate(forcing_halfhourly, vwind=2.0)), 
         forcing_acclim = list(mutate(forcing_acclim, vwind=2.0))) |> 
  ungroup()
save(p_hydro_drivers, file = "~/GitHub/geco-bern/rsofun/data/p_hydro_drivers.rda", compress = "xz")

load("~/GitHub/geco-bern/rsofun/data/p_model_drivers_vcmax25.rda")
p_model_drivers_vcmax25 <- p_model_drivers_vcmax25 |> 
  rowwise() |> mutate(forcing = list(mutate(forcing, vwind=2.0))) |> ungroup()
save(p_model_drivers_vcmax25, file = "~/GitHub/geco-bern/rsofun/data/p_model_drivers_vcmax25.rda", compress = "xz")

load("~/GitHub/geco-bern/rsofun/data/p_model_drivers.rda")
p_model_drivers <- p_model_drivers |> 
  rowwise() |> mutate(forcing = list(mutate(forcing, vwind=2.0))) |> ungroup()
save(p_model_drivers, file = "~/GitHub/geco-bern/rsofun/data/p_model_drivers.rda", compress = "bzip2")

load("~/GitHub/geco-bern/rsofun/data/p_model_drivers_format2024_08.rda")
p_model_drivers_format2025_02 <- p_model_drivers_format2024_08 |> 
  rowwise() |> mutate(forcing = list(mutate(forcing, vwind=2.0))) |> ungroup()
save(p_model_drivers_format2025_02, file = "~/GitHub/geco-bern/rsofun/data/p_model_drivers_format2025_02.rda", compress = "bzip2")
load("~/GitHub/geco-bern/rsofun/data/p_model_validation_format2024_08.rda")
p_model_validation_format2025_02 <- p_model_validation_format2024_08
save(p_model_validation_format2025_02, file = "~/GitHub/geco-bern/rsofun/data/p_model_validation_format2025_02.rda", compress = "bzip2")
```

TODO: what still needs to be updated in branch phydro are other information in the example drivers, namely params_siml (use_pml, use_gs, use_phydro) and site_info (canopy_height, reference_height):
```
rsofun::p_model_drivers_vcmax25 |>
    # TODO: NOT YET UPDATED FOR PHYDRO
    # # specify additionally needed params_siml flags:
    dplyr::mutate(params_siml = purrr::map(params_siml, \(x)
                                    dplyr::mutate(x,
                                           use_pml    = TRUE,
                                           use_gs     = TRUE,
                                           use_phydro = FALSE))) |>
    # specify additionally needed site info:
    dplyr::mutate(site_info = purrr::map(site_info, \(x)
                                  dplyr::mutate(x,
                                         canopy_height = 5,
                                         reference_height = 10)))
```
