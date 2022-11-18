## ----setup, include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(rsofun)
library(dplyr)
library(ggplot2)

pars <- list(par = c(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444,
    tau_acclim_tempstress = 7.35259044,
    par_shape_tempstress  = 0.09863961
  ))

## Modifications for PML  ------------------------------
p_model_drivers$site_info[[1]]$vegheight <- 25.0
p_model_drivers$site_info[[1]]$measureheight <- 30.0

p_model_drivers$forcing[[1]] <- p_model_drivers$forcing[[1]] |> 
  mutate(vwind = 1.0)  # wind speed in m/s

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# optimized parameters from previous
# work
params_modl <- list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )

# run the model for these parameters
output <- runread_pmodel_f(
  p_model_drivers,
  par = params_modl
  )


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we only have one site so we'll unnest
# the main model output
model_data <- output %>%
  filter(sitename == "FR-Pue") %>%
  tidyr::unnest(data)

validation_data <- p_model_validation %>%
  tidyr::unnest(data)

ggplot() +
    geom_line(
    data = model_data,
    aes(
      date,
      gpp
    ),
    colour = "red"
  ) +
  geom_line(
    data = validation_data,
    aes(
      date,
      gpp
    )
  ) +
  labs(
    x = "Date",
    y = "GPP"
  )


# ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# settings <- list(
#   method              = "bayesiantools",
#   targetvars          = c("gpp"),
#   timescale           = list(targets_obs = "y"),
#   metric              = cost_rmse_kphio,
#   dir_results         = "./",
#   name                = "ORG",
#   control = list(
#     sampler = "DEzs",
#     settings = list(
#       burnin = 500,
#       iterations = 1500
#     )),
#   par = list(
#     kphio = list(lower=0.04, upper=0.2, init = 0.05)
#     )
# )
# 
# 
# ## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ## # calibrate the model and
# ## # optimize free parameters
# ## pars <- calib_sofun(
# ##     drivers = p_model_drivers,
# ##     obs = p_model_validation,
# ##     settings = settings
# ##   )
# 
# 
# ## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # reformatting the parameter list
# params_modl <- list(
#     kphio = pars$par["kphio"]
#     )
# 
# # run the model for these parameters
# output_new <- rsofun::runread_pmodel_f(
#   p_model_drivers,
#   par = params_modl
#   )
# 
# # we only have one site so we'll unnest
# # the main model output
# model_data_new <- output_new %>%
#   filter(sitename == "FR-Pue") %>%
#   tidyr::unnest(data)
# 
#   ggplot() +
#     geom_line(
#     data = model_data,
#     aes(
#       date,
#       gpp
#     ),
#     colour = "blue",
#     alpha = 0.3
#   ) +
#   geom_line(
#     data = validation_data,
#     aes(
#       date,
#       gpp
#     )
#   ) +
#     geom_line(
#     data = model_data_new,
#     aes(
#       date,
#       gpp
#     ),
#     colour = "red"
#   ) +
#   labs(
#     x = "Date",
#     y = "GPP"
#   )

# validation_data %>% 
#   rename(obs = gpp) %>% 
#   left_join(
#     model_data_new %>% 
#       dplyr::select(sitename, date, mod = gpp),
#     by = c("sitename", "date")
#   ) %>% 
#   rbeni::analyse_modobs2("mod", "obs", type = "hex")

