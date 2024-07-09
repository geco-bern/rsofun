library(rsofun)
library(dplyr)
library(ggplot2)
library(purrr)

## BiomeE (original with gs-leuning) -----------
nruns <- 3
test_biomee_gs_leuning <- function(){
  # run the model
  biomee_gs_leuning_output <- runread_biomee_f(
    biomee_gs_leuning_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )

  # split out the annual data
  any(!is.na(biomee_gs_leuning_output$data[[1]]$output_annual_tile$GPP) & biomee_gs_leuning_output$data[[1]]$output_annual_tile$GPP > 0.0)
}

# TRUE if simulation passed
vec_test <- purrr::map_lgl(
  1:nruns,
  ~test_biomee_gs_leuning()
)

# did any simulation fail?
if (any(!vec_test)){
  stop(
    "At least one BiomeE-gs-leuning simulation failed."
  )
} else {
  message(
    "All BiomeE-gs-leuning simulations passed."
  )
}

## BiomeEP -------------------
nruns <- 10
test_biomeep <- function(){
  # run the model
  biomee_p_model_output <- runread_biomee_f(
    biomee_p_model_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # split out the annual data
  any(!is.na(biomee_p_model_output$data[[1]]$output_annual_tile$GPP) & biomee_p_model_output$data[[1]]$output_annual_tile$GPP > 0.0)
}

# TRUE if simulation passed
vec_test <- purrr::map_lgl(
  1:nruns,
  ~test_biomeep()
)

# did any simulation fail?
if (any(!vec_test)){
  stop(
    "At least one BiomeEP simulation failed."
  )
} else {
  message(
    "All BiomeEP simulations passed."
  )
}


# OLD BUG RELATED TO NET RADIATION AND EVAPOTRANSPIRATION - SOLVED BY NOT USING PRESCRIBED NET RADIATION IN COMBINATION WITH SPLASH
# library(rsofun)
# library(dplyr)
# library(ggplot2)
# 
# params_modl <- list(
#   kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
#   kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
#   kphio_par_b        = 1.0,
#   soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
#   soilm_betao        = 0.0,
#   beta_unitcostratio = 146.0,
#   rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#   tau_acclim         = 30.0,
#   kc_jmax            = 0.41
# )
# 
# # run the model for these parameters
# output <- rsofun::runread_pmodel_f(
#   p_model_drivers,
#   par = params_modl
# )
# 
# # problem with AET
# ggplot(aes(x = date),
#        data = output$data[[1]] |>
#          slice(1:365)) +
#   geom_line(aes(y = aet), color = "royalblue") +
#   geom_line(aes(y = pet), color = "tomato")
# 
# # net radiation in forcing
# p_model_drivers$forcing[[1]] |>
#   slice(1:365) |> 
#   ggplot(aes(date, netrad)) +
#   geom_line() +
#   labs(title = "in forcing")
# 
# # net radiation in output
# output$data[[1]] |>
#   slice(1:365) |> 
#   ggplot(aes(date, netrad)) +
#   geom_line() +
#   labs(title = "in output")
# 
# # they are identical - so far so good
# 
# # pet scales nicely with netrad
# output$data[[1]] |>
#   slice(1:365) |> 
#   ggplot(aes(netrad, pet)) +
#   geom_point()
# 
# # where the heck is the weird AET coming from???
# 
# # try with internally simulated net radiation
# output <- rsofun::runread_pmodel_f(
#   p_model_drivers |> 
#     mutate(forcing = purrr::map(forcing, ~mutate(., netrad = NA))),
#   par = params_modl
# )
# 
# # problem with AET
# ggplot(aes(x = date),
#        data = output$data[[1]] |>
#          slice(1:365)) +
#   geom_line(aes(y = aet), color = "royalblue") +
#   geom_line(aes(y = pet), color = "tomato")
# 
# # compare observed and splash netrad
# 
# ggplot() +
#   geom_line(aes(date, netrad),
#             data = p_model_drivers$forcing[[1]] |>
#               slice(1:365)) +
#   geom_line(aes(date, netrad),
#             data = output$data[[1]] |>
#               slice(1:365),
#             color = "red")
# 
# # when using prescirbed, splash-internal netrad is -2.51781612E+10
# # when internally calculating,  splash-internal netrad is 4283690.50  
# # interstingly, PET is still ok and seems to use correct net radiation
# 
# # tau, tile_fluxes(:)%canopy%rnl   0.755406737       98.3971481  
# # tau, tile_fluxes(:)%canopy%rnl   0.755406737       98.3971481    

