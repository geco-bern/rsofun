library(rsofun)
library(rpmodel)
library(tidyverse)

df <- rsofun::p_model_drivers

# set model drivers to the NPHT paper
# ones
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  df,
  par = params_modl
)$data[[1]]$gpp

df <- df$forcing[[1]]

output_rp <- apply(df, 1, function(x){
  out <- rpmodel::rpmodel(
    tc             = as.numeric(x['temp']),
    patm           = as.numeric(x['patm']),
    co2            = as.numeric(x['co2']),
    fapar          = as.numeric(x['fapar']),
    ppfd           = as.numeric(x['ppfd']),
    vpd            = as.numeric(x['vpd']),
    elv            = 270,
    kphio          = 0.09423773,
    beta           = 145,
    c4             = FALSE,
    method_optci   = "prentice14",
    method_jmaxlim = "wang17",
    do_ftemp_kphio = TRUE,
    do_soilmstress = FALSE,
    verbose        = TRUE
  )
})

output_rp <- data.frame(do.call("rbind", output_rp))
output_rp <- unlist(output_rp$gpp)

par(mfrow = c(2,1))
plot(output_rp)
plot(output)

# normal tolerance ~ 0.67
tolerance <- mean(abs(output - gpp), na.rm = TRUE)/
  mean(abs(gpp), na.rm = TRUE)

# test for correctly returned values
expect_equal(tolerance, 0.6768124, tolerance = 0.03)