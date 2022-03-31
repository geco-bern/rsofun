library(rsofun)
library(dplyr)

set.seed(10)

df_drivers <- lm3ppa_gs_leuning_drivers
ddf_obs <- lm3ppa_validation_2
df_drivers$params_siml[[1]]$spinup <- FALSE

out <- lapply(1:20000, function(x){
    print(sprintf("--- call %05d ----", x))
    # run model
    df <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )

  invisible(x)
})

