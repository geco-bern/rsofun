library(rsofun)
library(dplyr)

set.seed(10)

df_drivers <- bimee_gs_leuning_drivers
ddf_obs <- bimee_validation_2
df_drivers$params_siml[[1]]$spinup <- FALSE

out <- lapply(1:20000, function(x){
    print(sprintf("--- call %05d ----", x))
    # run model
    df <- runread_bimee_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )

  invisible(x)
})

