# convert data fields

library(rsofun)

p_model_drivers$params_soil <- p_model_drivers$soil_texture
p_model_drivers <- p_model_drivers %>%
  select(
    -soil_texture
  )

save(p_model_drivers,
     file ="data/p_model_drivers.rda",
     compress = "xz")

convert_p_model_drivers <- function(df) {
  df$site_info <- df$siteinfo
  df$params_soil <- df$df_soiltexture
  df <- df %>%
    select(
      -siteinfo, -df_soiltexture
    )
  return(df)
}
