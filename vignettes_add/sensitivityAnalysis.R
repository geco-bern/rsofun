
###### Sensitivity analysis for calibratable parameters ###### 

##### Run with pmodel ##### 

######  kphio interval

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

kphio_values <- seq(0.02, 0.1, 0.001)

kphio_eval_interval <- data.frame()
#kphio_eval_interval_all <- data.frame()

for(i in kphio_values){
  
  df_drivers$params_species[[1]]$kphio  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  kphio_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #kphio_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  kphio_eval_interval <- rbind(kphio_eval_interval,kphio_eval_rows)
  #kphio_eval_interval_all <- rbind(kphio_eval_interval_all,kphio_eval_rows_all)
  
}

save(kphio_eval_interval, file = "~/data/rsofun/kphio_eval_interval_pmodel_dbh.RData")

### kphio zoom

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

kphio_values <- seq(0.0400001, 0.0400002, 1e-09)

kphio_eval_zoom <- data.frame()
#kphio_eval_interval_all <- data.frame()

for(i in kphio_values){
  
  df_drivers$params_species[[1]]$kphio  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  kphio_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #kphio_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  kphio_eval_zoom <- rbind(kphio_eval_zoom,kphio_eval_rows)
  #kphio_eval_interval_all <- rbind(kphio_eval_interval_all,kphio_eval_rows_all)
  
}

save(kphio_eval_zoom, file = "~/data/rsofun/kphio_eval_zoom_pmodel_dbh.RData")

### phiRL interval

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

phiRL_values <- seq(0.1, 12, 0.1)

phiRL_eval_interval <- data.frame()
#phiRL_eval_interval_all <- data.frame()

for(i in phiRL_values){
  
  df_drivers$params_species[[1]]$phiRL  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  phiRL_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #phiRL_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  phiRL_eval_interval <- rbind(phiRL_eval_interval,phiRL_eval_rows)
  #phiRL_eval_interval_all <- rbind(phiRL_eval_interval_all,phiRL_eval_rows_all)
  
}

save(phiRL_eval_interval, file = "~/data/rsofun/phiRL_eval_interval_pmodel_dbh.RData")

### phiRL zoom

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

phiRL_values <- seq(3.500001, 3.500002, 1e-08)

phiRL_eval_zoom <- data.frame()
#phiRL_eval_interval_all <- data.frame()

for(i in phiRL_values){
  
  df_drivers$params_species[[1]]$phiRL  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  phiRL_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #phiRL_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  phiRL_eval_zoom <- rbind(phiRL_eval_zoom,phiRL_eval_rows)
  #phiRL_eval_zoom_all <- rbind(phiRL_eval_zoom_all,phiRL_eval_rows_all)
  
}

save(phiRL_eval_zoom, file = "~/data/rsofun/phiRL_eval_zoom_pmodel_dbh.RData")

### LAI_light interval

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

LAI_light_values <- seq(0.1, 12, 0.1)

LAI_light_eval_interval <- data.frame()
#LAI_light_eval_interval_all <- data.frame()

for(i in LAI_light_values){
  
  df_drivers$params_species[[1]]$LAI_light  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  LAI_light_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #LAI_light_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  LAI_light_eval_interval <- rbind(LAI_light_eval_interval,LAI_light_eval_rows)
  #LAI_light_eval_interval_all <- rbind(LAI_light_eval_interval_all,LAI_light_eval_rows_all)
  
}

save(LAI_light_eval_interval, file = "~/data/rsofun/LAI_light_eval_interval_pmodel_dbh.RData")

### LAI_light zoom

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

LAI_light_values <- seq(3.500001, 3.500002, 1e-08)

LAI_light_eval_zoom <- data.frame()
#LAI_light_eval_interval_all <- data.frame()

for(i in LAI_light_values){
  
  df_drivers$params_species[[1]]$LAI_light  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  LAI_light_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #LAI_light_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  LAI_light_eval_zoom <- rbind(LAI_light_eval_zoom,LAI_light_eval_rows)
  #LAI_light_eval_zoom_all <- rbind(LAI_light_eval_zoom_all,LAI_light_eval_rows_all)
  
}

save(LAI_light_eval_zoom, file = "~/data/rsofun/LAI_light_eval_zoom_pmodel_dbh.RData")

### tf_base interval

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

tf_base_values <- seq(0.1, 12, 0.1)

tf_base_eval_interval <- data.frame()
#tf_base_eval_interval_all <- data.frame()

for(i in tf_base_values){
  
  df_drivers$params_tile[[1]]$tf_base  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  tf_base_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #tf_base_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  tf_base_eval_interval <- rbind(tf_base_eval_interval,tf_base_eval_rows)
  #tf_base_eval_interval_all <- rbind(tf_base_eval_interval_all,tf_base_eval_rows_all)
  
}

save(tf_base_eval_interval, file = "~/data/rsofun/tf_base_eval_interval_pmodel_dbh.RData")

### tf_base zoom

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

tf_base_values <- seq(1.000001, 1.000002, 1e-08)

tf_base_eval_zoom <- data.frame()
#tf_base_eval_interval_all <- data.frame()

for(i in tf_base_values){
  
  df_drivers$params_tile[[1]]$tf_base  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  tf_base_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #tf_base_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  tf_base_eval_zoom <- rbind(tf_base_eval_zoom,tf_base_eval_rows)
  #tf_base_eval_zoom_all <- rbind(tf_base_eval_zoom_all,tf_base_eval_rows_all)
  
}

save(tf_base_eval_zoom, file = "~/data/rsofun/tf_base_eval_zoom_pmodel_dbh.RData")

### par_mort interval

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

par_mort_values <- seq(0.01, 1, 0.01)

par_mort_eval_interval <- data.frame()
#par_mort_eval_interval_all <- data.frame()

for(i in par_mort_values){
  
  df_drivers$params_tile[[1]]$par_mort  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  par_mort_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #par_mort_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  par_mort_eval_interval <- rbind(par_mort_eval_interval,par_mort_eval_rows)
  #par_mort_eval_interval_all <- rbind(par_mort_eval_interval_all,par_mort_eval_rows_all)
  
}

save(par_mort_eval_interval, file = "~/data/rsofun/par_mort_eval_interval_pmodel_dbh.RData")

### par_mort zoom

load("~/data/rsofun/df_drivers_pmodel_dbh.RData")

par_mort_values <- seq(0.600001, 0.600002, 1e-08)

par_mort_eval_zoom <- data.frame()
#par_mort_eval_interval_all <- data.frame()

for(i in par_mort_values){
  
  df_drivers$params_tile[[1]]$par_mort  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  par_mort_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #par_mort_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  par_mort_eval_zoom <- rbind(par_mort_eval_zoom,par_mort_eval_rows)
  #par_mort_eval_zoom_all <- rbind(par_mort_eval_zoom_all,par_mort_eval_rows_all)
  
}

save(par_mort_eval_zoom, file = "~/data/rsofun/par_mort_eval_zoom_pmodel_dbh.RData")



##### Run with gs-Leuning ##### 

######  No kphio in gs

### phiRL interval

load("~/data/rsofun/df_drivers_gs_dbh.RData")

phiRL_values <- seq(0.1, 12, 0.1)

phiRL_eval_interval <- data.frame()
#phiRL_eval_interval_all <- data.frame()

for(i in phiRL_values){
  
  df_drivers$params_species[[1]]$phiRL  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  phiRL_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #phiRL_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  phiRL_eval_interval <- rbind(phiRL_eval_interval,phiRL_eval_rows)
  #phiRL_eval_interval_all <- rbind(phiRL_eval_interval_all,phiRL_eval_rows_all)
  
}

save(phiRL_eval_interval, file = "~/data/rsofun/phiRL_eval_interval_gs_dbh.RData")

### phiRL zoom

load("~/data/rsofun/df_drivers_gs_dbh.RData")

phiRL_values <- seq(3.500001, 3.500002, 1e-08)

phiRL_eval_zoom <- data.frame()
#phiRL_eval_interval_all <- data.frame()

for(i in phiRL_values){
  
  df_drivers$params_species[[1]]$phiRL  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  phiRL_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #phiRL_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  phiRL_eval_zoom <- rbind(phiRL_eval_zoom,phiRL_eval_rows)
  #phiRL_eval_zoom_all <- rbind(phiRL_eval_zoom_all,phiRL_eval_rows_all)
  
}

save(phiRL_eval_zoom, file = "~/data/rsofun/phiRL_eval_zoom_gs_dbh.RData")

### LAI_light interval

load("~/data/rsofun/df_drivers_gs_dbh.RData")

LAI_light_values <- seq(0.1, 12, 0.1)

LAI_light_eval_interval <- data.frame()
#LAI_light_eval_interval_all <- data.frame()

for(i in LAI_light_values){
  
  df_drivers$params_species[[1]]$LAI_light  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  LAI_light_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #LAI_light_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  LAI_light_eval_interval <- rbind(LAI_light_eval_interval,LAI_light_eval_rows)
  #LAI_light_eval_interval_all <- rbind(LAI_light_eval_interval_all,LAI_light_eval_rows_all)
  
}

save(LAI_light_eval_interval, file = "~/data/rsofun/LAI_light_eval_interval_gs_dbh.RData")

### LAI_light zoom

load("~/data/rsofun/df_drivers_gs_dbh.RData")

LAI_light_values <- seq(3.500001, 3.500002, 1e-08)

LAI_light_eval_zoom <- data.frame()
#LAI_light_eval_interval_all <- data.frame()

for(i in LAI_light_values){
  
  df_drivers$params_species[[1]]$LAI_light  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  LAI_light_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #LAI_light_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  LAI_light_eval_zoom <- rbind(LAI_light_eval_zoom,LAI_light_eval_rows)
  #LAI_light_eval_zoom_all <- rbind(LAI_light_eval_zoom_all,LAI_light_eval_rows_all)
  
}

save(LAI_light_eval_zoom, file = "~/data/rsofun/LAI_light_eval_zoom_gs_dbh.RData")

### tf_base interval

load("~/data/rsofun/df_drivers_gs_dbh.RData")

tf_base_values <- seq(0.1, 12, 0.1)

tf_base_eval_interval <- data.frame()
#tf_base_eval_interval_all <- data.frame()

for(i in tf_base_values){
  
  df_drivers$params_tile[[1]]$tf_base  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  tf_base_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #tf_base_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  tf_base_eval_interval <- rbind(tf_base_eval_interval,tf_base_eval_rows)
  #tf_base_eval_interval_all <- rbind(tf_base_eval_interval_all,tf_base_eval_rows_all)
  
}

save(tf_base_eval_interval, file = "~/data/rsofun/tf_base_eval_interval_gs_dbh.RData")

### tf_base zoom

load("~/data/rsofun/df_drivers_gs_dbh.RData")

tf_base_values <- seq(1.000001, 1.000002, 1e-08)

tf_base_eval_zoom <- data.frame()
#tf_base_eval_interval_all <- data.frame()

for(i in tf_base_values){
  
  df_drivers$params_tile[[1]]$tf_base  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  tf_base_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #tf_base_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  tf_base_eval_zoom <- rbind(tf_base_eval_zoom,tf_base_eval_rows)
  #tf_base_eval_zoom_all <- rbind(tf_base_eval_zoom_all,tf_base_eval_rows_all)
  
}

save(tf_base_eval_zoom, file = "~/data/rsofun/tf_base_eval_zoom_gs_dbh.RData")

### par_mort interval

load("~/data/rsofun/df_drivers_gs_dbh.RData")

par_mort_values <- seq(0.01, 1, 0.01)

par_mort_eval_interval <- data.frame()
#par_mort_eval_interval_all <- data.frame()

for(i in par_mort_values){
  
  df_drivers$params_tile[[1]]$par_mort  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  par_mort_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #par_mort_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  par_mort_eval_interval <- rbind(par_mort_eval_interval,par_mort_eval_rows)
  #par_mort_eval_interval_all <- rbind(par_mort_eval_interval_all,par_mort_eval_rows_all)
  
}

save(par_mort_eval_interval, file = "~/data/rsofun/par_mort_eval_interval_gs_dbh.RData")

### par_mort zoom

load("~/data/rsofun/df_drivers_gs_dbh.RData")

par_mort_values <- seq(0.600001, 0.600002, 1e-08)

par_mort_eval_zoom <- data.frame()
#par_mort_eval_interval_all <- data.frame()

for(i in par_mort_values){
  
  df_drivers$params_tile[[1]]$par_mort  <- i
  
  df_calib_DBH <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  par_mort_eval_rows <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>%
    summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC)) %>%
    mutate(Param_val = i)
  
  #par_mort_eval_rows_all <-  df_calib_DBH$data[[1]]$output_annual_tile  %>% tail(500) %>% dplyr::select(GPP,LAI,Density12,plantC) %>% mutate(Param_val = i)
  
  par_mort_eval_zoom <- rbind(par_mort_eval_zoom,par_mort_eval_rows)
  #par_mort_eval_zoom_all <- rbind(par_mort_eval_zoom_all,par_mort_eval_rows_all)
  
}

save(par_mort_eval_zoom, file = "~/data/rsofun/par_mort_eval_zoom_gs_dbh.RData")



