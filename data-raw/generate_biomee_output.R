#!/usr/bin/env Rscript

library(tidyverse)
library(rsofun)

generate_output_annual_tile <- function(drivers) {
  out <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE)

  output_annual_tile <- out$data[[1]]$output_annual_tile
  output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

  cowplot::plot_grid(
    output_annual_tile %>%
      ggplot() +
      geom_line(aes(x = year, y = GPP)) +
      theme_classic()+labs(x = "Year", y = "GPP"),
    output_annual_tile %>%
      ggplot() +
      geom_line(aes(x = year, y = plantC)) +
      theme_classic()+labs(x = "Year", y = "plantC")
  )

  output_annual_cohorts %>% group_by(PFT,year) %>%
    summarise(npp=sum(NPP*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
    ggplot() +
    geom_line(aes(x = year, y = npp,col=PFT)) +
    theme_classic()+labs(x = "Year", y = "NPP")

  return(output_annual_tile)
}

# run the model gs-leuning
biomee_gs_leuning_output <- generate_output_annual_tile(biomee_gs_leuning_drivers)

save(biomee_gs_leuning_output,
     file ="data/biomee_gs_leuning_output.rda",
     compress = "xz")

# run the model p-model
biomee_p_model_output <- generate_output_annual_tile(biomee_p_model_drivers)

save(biomee_p_model_output,
     file = "data/biomee_p_model_output.rda",
     compress = "xz")
