## ---
## title: "W-model site-scale simulations"
## author: "Benjamin D. Stocker"
## ---

# Environment
library(rsofun)
library(rbeni)

load_dependencies_rsofun()
# systr <- "''"    # for Mac
knitr::opts_knit$set( root.dir = rprojroot::find_rstudio_root_file() ) # does not work properly
if (!file.exists("bash"))    system("ln -s inst/bash bash")
if (!file.exists("extdata")) system("ln -s inst/extdata extdata")
options( list( rsofun.dir.sofun="~/sofun/trunk/" ) )


## Simulation settings
siteinfo <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  write_csv(path = "./siteinfo_pet_fluxnet2015.csv")

settings_sims_sitescale <- list(
  path_siteinfo   = "./siteinfo_pet_fluxnet2015.csv",
  ensemble        = TRUE,
  setup           = "site",
  name            = "pet_fluxnet2015",
  dir_sofun       = options()$rsofun.dir.sofun,
  path_output     = "~/sofun/output_pet_fluxnet2015_sofun/",
  path_output_nc  = "~/sofun/output_nc_pet_fluxnet2015_sofun/",
  path_input      = "~/sofun/input_pet_fluxnet2015_sofun/",
  grid            = NA,
  implementation  = "fortran",
  in_ppfd         = TRUE,
  in_netrad       = FALSE,
  recycle         = 1,
  spinupyears     = 10,
  calibvars       = c(),  # needed later for calibration setup, any of "gpp", "fapar", and "transp"
  soilmstress     = TRUE,
  tempstress      = TRUE,
  loutdgpp        = FALSE,
  loutdwcont      = TRUE,
  loutdaet        = TRUE,
  loutdpet        = TRUE,
  loutdnetrad     = TRUE,
  loutdwbal       = TRUE
  )


## Input settings
settings_input_sitescale <-  list(
  data                     = NA,
  temperature              = "fluxnet2015",
  precipitation            = "fluxnet2015",
  vpd                      = "fluxnet2015",
  ppfd                     = "fluxnet2015",
  netrad                   = "fluxnet2015",  #  c("fluxnet2015", "watch_wfdei"),
  patm                     = "fluxnet2015",
  cloudcover               = "cru",
  fapar                    = "MODIS_FPAR_MCD15A3H",
  splined_fapar            = FALSE,
  path_co2                 = "~/data/co2/cCO2_rcp85_const850-1765.dat",
  path_fluxnet2015         = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/",
  path_fluxnet2015_hh      = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_0.5h/original/unpacked/",
  path_MODIS_FPAR_MCD15A3H = "~/data/fapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_fluxnet2015_gee_subset/",
  get_from_remote          = FALSE,
  path_cru                 = "~/data/cru/ts_4.01/"
  )


## Model setup
# Define model setup as a list.

setup_sofun_sitescale <- list(
  model      = "swbm",
  dir        = options()$rsofun.dir.sofun,
  do_compile = FALSE,
  simsuite   = FALSE
  )

## Workflow

## Prepare simulation setup
settings_sims_sitescale <- prepare_setup_sofun(
  settings = settings_sims_sitescale,
    setup = setup_sofun_sitescale,
  write_paramfils = TRUE
  )

## Prepare inputs
inputdata <- prepare_input_sofun(
  settings_input = settings_input_sitescale,
  settings_sims = settings_sims_sitescale,
  return_data = TRUE,
  overwrite_climate = TRUE,
  overwrite_fapar = TRUE,
  verbose = TRUE,
  overwrite_csv_climate = TRUE,
  overwrite_csv_fapar = TRUE
  )


# ## Run the model
# settings_sims_sitescale$in_netrad <- FALSE
# nothing <- update_params( 
#   params_opt, 
#   settings_sims_sitescale$dir_sofun, 
#   setup = setup_sofun_sitescale 
#   )

# ## run and read at once
# mod <- runread_sofun(
#   settings = settings_sims_sitescale,
#   setup = setup_sofun_sitescale
#   )

# ## read only
# mod <- read_sofun(
#   settings = settings_sims_sitescale,
#   setup = setup_sofun_sitescale
#   )
# save(mod, file = "~/mct/data/mod.RData")


# ## Evaluate the model
# settings_eval <- list(
#   sitenames = rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015$sitename,
#   benchmark = list( aet = c("fluxnet2015"), netrad = c("fluxnet2015") ),
#   sitenames_siteplots = "FR-Pue",
#   agg = 5,
#   path_fluxnet2015_d = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/",
#   path_fluxnet2015_w = "",
#   path_fluxnet2015_m = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/",
#   path_fluxnet2015_y = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/",
#   path_gepisat_d     = "",
#   dir_figs           = "~/mct/fig/",
#   remove_premodis    = FALSE
#   )


# ## Get observational data for evaluation
# obs_eval <- get_obs_eval( 
#   settings_eval = settings_eval, 
#   settings_sims = settings_sims_sitescale, 
#   overwrite = TRUE 
#   )

# ## Evaluate the whole shabang
# out_eval <- eval_sofun(
#   mod, 
#   settings_eval, 
#   settings_sims_sitescale, 
#   obs_eval = obs_eval, 
#   overwrite = TRUE 
#   )

# out_plot <- out_eval$netrad$fluxnet2015$data$xdf %>% 
#   mutate(mod=mod/(60*60*24), obs=obs/(60*60*24)) %>% 
#   analyse_modobs2("mod", "obs", type="heat")

# out_plot <- out_eval$aet$fluxnet2015$data$xdf %>% 
#   analyse_modobs2("mod", "obs", type="heat")

# out_plot$gg + 
#   labs(x=expression(paste("Modelled net radiation (W m"^-2, ")")), 
#        y=expression(paste("Simulated net radiation (W m"^-2, ")")))

# ggsave("fig/modobs_netrad.pdf")

# out_eval$aet$fluxnet2015$plot$by_doy_allsites()

# save(obs_eval, file = "~/mct/data/obs_eval.RData")
# save(out_eval, file = "~/mct/data/out_eval.RData")


# # modobs_daily   <- out_eval$aet$fluxnet2015$plot$modobs_daily()
# # modobs_xdf     <- out_eval$aet$fluxnet2015$plot$modobs_xdaily()
# # modobs_mdf     <- out_eval$aet$fluxnet2015$plot$modobs_monthly()
# # modobs_meandoy <- out_eval$aet$fluxnet2015$plot$modobs_meandoy()
# # modobs_meandoy <- out_eval$aet$fluxnet2015$plot$modobs_annual()
# # out_eval$aet$fluxnet2015$plot$by_doy_allsites()
# # out_eval$aet$fluxnet2015$plot$by_xoy_allsites()
# # modobs_anomalies_daily  <- out_eval$aet$fluxnet2015$plot$modobs_anomalies_daily()
# # modobs_anomalies_xdaily <- out_eval$aet$fluxnet2015$plot$modobs_anomalies_xdaily()
# # modobs_anomalies_annual <- out_eval$aet$fluxnet2015$plot$modobs_anomalies_annual()
# # modobs_meanxoy <- out_eval$aet$fluxnet2015$plot$modobs_meanxoy()
# # out_eval$aet$fluxnet2015$plot$by_doy_allzones()
# # #out_eval$aet$fluxnet2015$plot$modobs_spatial_annual()
# # modobs_spatial <- out_eval$aet$fluxnet2015$plot$modobs_spatial()

