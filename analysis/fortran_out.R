
biomee_output_annual_tile <- read.csv("/home/laura/rsofun/output/Annual_tile_test.csv",sep=",", header = F)
biomee_output_annual_tile
str(biomee_output_annual_tile)
biomee_output_annual_cohorts <- read.csv("/home/laura/rsofun/output/Annual_cohorts_test.csv",sep=",", header = F)
biomee_output_annual_cohorts
str(biomee_output_annual_cohorts)


cowplot::plot_grid(
  biomee_gs_leuning_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = GPP)) +
    theme_classic()+labs(x = "Year", y = "GPP"),
  biomee_gs_leuning_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = plantC)) +
    theme_classic()+labs(x = "Year", y = "plantC")
)