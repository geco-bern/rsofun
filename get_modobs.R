####################################################
## Reads in model output data and observational data
## from FLUXNET and arranges it all by site with:
## fluxnet$<sitename>$ddf$obs  $[data frame]
##                       $<sim>$[data frame]
##                   $ddf_stat$rmse
## etc.
##
## Units:
## GPP : gC m-2 d-1
## ET  : J m-2 d-1
## SWC : 0
## 
####################################################
.libPaths( c( .libPaths(), "/home/bstocker/R/x86_64-pc-linux-gnu-library/3.3") )

library(dplyr)
library(readr)

##--------------------------------------------------------------------
## MANUAL SETTINGS
##--------------------------------------------------------------------
myhome               = "~/"
simsuite             = "fluxnet2015"  
outputset            = c( "s14" )
add_swcvars          = TRUE
add_swcvars_etbucket = FALSE
overwrite            = TRUE
overwrite_dosites    = TRUE 
outdir               = "~/data/fluxnet_sofun/"
##--------------------------------------------------------------------

## get from other repository 'utilities'
source( paste0( myhome, "utilities/conv_noleap_to_ymd.R" ) )

source("get_daily_modelout.R")
source("add_swcvars_fluxnet2015.R")
source("get_modobs_fluxnet2015.R")

siteinfo   <- read_csv( paste0( myhome, "sofun/input_fluxnet2015_sofun/siteinfo_", simsuite, "_sofun.csv" ) )
datafilnam <- paste0( "modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v4.Rdata" )

if (overwrite){

  ## re-do all sites selected above (by do.sites)
  do.sites_eff <- seq(nrow(siteinfo))
  fluxnet <- list()
  missing_2015 <- c()
  missing_mod  <- list()
  missing_inclim <- c()
  missing_inevi <- c()
  missing_infpar <- c()

} else {

  if (overwrite_dosites) {
    
    do.sites_eff <- do.sites
  
  } else {
    ## do only sites that are missing in the list 'fluxnet' loaded from 'datafilnam'
    load( datafilnam )
    allsites <- as.character( siteinfo$mysitename )[ do.sites ]
    avl <- ls(fluxnet)
    addsites <- setdiff(allsites, avl)
    do.sites_eff <- which( is.element( allsites, addsites ) )
  
  }

}

do.sites_names <- as.character( siteinfo$mysitename[do.sites_eff] )

# ##==========================================================
# ##----------------------------------------------------------
# ## Evaluate arguments provided by R CMD BATCH call
# ##----------------------------------------------------------
# ## First read in the arguments listed at the command line.
# ## Call this by 
# ## 'R CMD BATCH --no-save --no-restore '--args sitename="FR-Pue" nam_target="lue_obs"' profile_soilm_neuralnet.R profile_soilm_neuralnet.out &'
# args=(commandArgs(TRUE))

# ## args is now a list of character vectors
# ## First check to see if arguments are passed.
# ## Then cycle through each element of the list and evaluate the expressions.
# if (length(args)==0){
#   print("No arguments supplied. Provide at least sitename.")
# } else {
#   for (i in 1:length(args)){
#      eval( parse( text=args[[i]] ) )
#   }
# }
# ##----------------------------------------------------------
# ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

for (sitename in do.sites_names){

  #---------------------------------------------------------
  # Get model output and input data
  #---------------------------------------------------------
  fluxnet <- get_modobs_fluxnet2015( 
                                    sitename, 
                                    simsuite          = simsuite,
                                    outputset         = outputset,
                                    data              = fluxnet,
                                    getvars           = c( "gpp", "wcont", "aet", "pet" ), 
                                    add_swcvars       = add_swcvars, 
                                    overwrite         = TRUE, 
                                    overwrite_dosites = TRUE,
                                    outdir            = outdir
    )  

  #---------------------------------------------------------
  # Add obs-ET-driven soil moisture data
  #---------------------------------------------------------
  if (add_swcvars_etbucket){

    fluxnet <- add_swcvars_fluxnet2015( sitename, fluxnet=fluxnet, outdir=outdir )

  }

  # #---------------------------------------------------------
  # # Plot ET with gaps (data from FLUXNET2015) and my gap-filled ET
  # #---------------------------------------------------------
  # ddf <- fluxnet[[ sitename ]]$ddf

  # ## plot for verification
  # na_idxs <- which( is.na(ddf$obs$le_f_mds) )
  # hasgaps <- length( na_idxs ) > 0
  # plot( ddf$obs$year_dec,  ddf$obs$le_f_mds * 1e-6, type="l", xlab="year", ylab="ET (MJ m-2)", main=sitename ) ## with gaps
  # if (hasgaps) { 
  #   # plotvec <- ddf$obs$le_f_mds_mygapfilled * 1e-6
  #   # plotvec[ -na_idxs ] <- NA
  #   lines( ddf$obs$year_dec,  ddf$obs$le_f_mds_mygapfilled * 1e-6, col='red' ) 
  #   lines( ddf$obs$year_dec,  ddf$obs$le_f_mds * 1e-6 ) 
  # }  ## gap-filled

}

#---------------------------------------------------------
# Save complete data to single Rdata file
#---------------------------------------------------------
save( fluxnet, missing_2015, missing_mod, missing_inclim, missing_inevi, file=datafilnam )
print("done saving.")





