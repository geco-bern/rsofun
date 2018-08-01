


source("get_modobs_fluxnet2015.R")

cost_function <- function( par ){
  cost = system( paste0("echo fcover ", sprintf( "%f", par ), " | ./runcmodel_simsuite | tail -n 1"), intern = TRUE)
  return((as.numeric(cost)))
}

##---------------------------------------------------------
## Calibrate model
##---------------------------------------------------------

## Model settings
settings_model <- list( model="pmodel_simsuite", do_compile=FALSE, simsuite="fluxnet2015" )





## Observations settings
settings_obs <- list( 
									simsuite = "fluxnet2015", 
									outputset = "s15", 
									outdir = "~/data/fluxnet_sofun/", 
									add_swcvars = TRUE
									)


do.call( get_modobs_fluxnet2015, do.sites_names )

fluxnet <- list()
sitename <- "FR-Pue"
myhome <- "~/"
fluxnet <- get_modobs_fluxnet2015( 
                                  sitename, 
                                  simsuite          = settings$simsuite,
                                  outputset         = settings$outputset,
                                  list_modobs       = fluxnet,
                                  getvars           = c( "gpp", "wcont", "aet", "pet" ), 
                                  add_swcvars       = add_swcvars, 
                                  overwrite         = TRUE, 
                                  overwrite_dosites = TRUE,
                                  outdir            = settings$outdir
																  )  
