
biomee_output_annual_tile <- read.csv("/home/laura/rsofun/output/Annual_cohorts_test.csv",sep=",", header = FALSE)
biomee_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

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