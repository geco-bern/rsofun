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
get_modobs <- function( simsuite, outputset, outdir="./", add_swcvars=TRUE, add_swcvars_etbucket=FALSE, overwrite=TRUE, overwrite_dosites=TRUE ){

  require(dplyr)
  require(readr)
  require(ggplot2)

  # ## debug --------------------
  # simsuite = "fluxnet2015"
  # outputset = c( "s15" )
  # outdir = "~/data/fluxnet_sofun/" 
  # add_swcvars = TRUE
  # add_swcvars_etbucket = FALSE
  # overwrite = TRUE
  # overwrite_dosites = TRUE
  # ##---------------------------

  ## get from other repository 'utilities'
  source( paste0( myhome, "utilities/conv_noleap_to_ymd.R" ) )

  source("get_daily_modelout.R")
  source("add_swcvars_fluxnet2015.R")
  source("get_modobs_fluxnet2015.R")

  siteinfo   <- read_csv( paste0( myhome, "sofun/input_fluxnet2015_sofun/siteinfo_", simsuite, "_sofun.csv" ) )
  
  datafilnam <- paste0( "modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v5" )

  filn_ddf <- paste0( "ddf_modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v5" )
  filn_wdf <- paste0( "wdf_modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v5" )
  filn_mdf <- paste0( "mdf_modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v5" )
  filn_adf <- paste0( "adf_modobs_fluxnet2015_", paste( outputset, collapse="_"), "_with_SWC_v5" )

  ## Exclude sites for which no fapar data is available
  df_error_fapar <- read_csv( paste0( myhome, "sofun/input_fluxnet2015_sofun/error_missing_forcingdata_MODIS_FPAR_MCD15A3H_fluxnet2015.csv" ) ) 
  siteinfo <- siteinfo %>% left_join( df_error_fapar, by="mysitename") %>% rename( error_fapar = error ) %>% filter( error_fapar == 0 )

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

  for (sitename in do.sites_names){

    #---------------------------------------------------------
    # Get model output and input data
    #---------------------------------------------------------
    print( paste( "Getting data for site ", sitename ) )
    fluxnet <- get_modobs_fluxnet2015( 
                                      sitename, 
                                      simsuite          = simsuite,
                                      outputset         = outputset,
                                      list_modobs       = fluxnet,
                                      getvars           = c( "gpp", "wcont", "aet", "pet" ), 
                                      add_swcvars       = add_swcvars, 
                                      overwrite         = TRUE, 
                                      overwrite_dosites = TRUE,
                                      outdir            = outdir
      )  

    # #---------------------------------------------------------
    # # Add obs-ET-driven soil moisture data
    # #---------------------------------------------------------
    # if (add_swcvars_etbucket){
    # 
    #   fluxnet <- add_swcvars_fluxnet2015( sitename, fluxnet=fluxnet, outdir=outdir )
    # 
    # }

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
  # Re-arrange data into a single flat dataframe (only implemented for using single SOFUN outputset)
  #---------------------------------------------------------
  print("converting to flat data frame (tibble)...")
  ddf_fluxnet <- tibble()
  wdf_fluxnet <- tibble()
  mdf_fluxnet <- tibble()
  adf_fluxnet <- tibble()

  ## daily
  for (sitename in do.sites_names){
    if ( ncol(fluxnet[[sitename]]$ddf[[ outputset ]])>0 ){

      ## combine separate dataframes into single one 
      ddf_site <- fluxnet[[ sitename ]]$ddf$inp %>% mutate( mysitename=sitename ) %>%
                                                    left_join( fluxnet[[ sitename ]]$ddf$obs, by = "date" ) %>%
                                                    left_join( fluxnet[[ sitename ]]$ddf$swc_obs, by = "date" ) %>%
                                                    left_join( fluxnet[[ sitename ]]$ddf[[ outputset ]], by = "date" )

      ## add mean of soil moisture across observational and model data ('soilm_mean')
      ddf_site <- ddf_site %>% mutate( soilm_mean = apply( select( ., starts_with("SWC_F_MDS"), wcont ), 1, FUN = mean, na.rm = TRUE ) )

      ddf_fluxnet <- ddf_fluxnet %>% bind_rows( ddf_site )
    
    }
  }

  ## weekly
  for (sitename in do.sites_names){
    if ( ncol(fluxnet[[sitename]]$wdf[[ outputset ]])>0 ){

      ## combine separate dataframes into single one 
      wdf_site <- fluxnet[[ sitename ]]$wdf$inp %>% mutate( mysitename=sitename ) %>%
                                                    left_join( fluxnet[[ sitename ]]$wdf$obs, by = "date" ) %>%
                                                    left_join( fluxnet[[ sitename ]]$wdf[[ outputset ]], by = "date" )

      wdf_fluxnet <- wdf_fluxnet %>% bind_rows( wdf_site )
    
    }
  }

  ## monthly
  for (sitename in do.sites_names){
    if ( ncol(fluxnet[[sitename]]$mdf[[ outputset ]])>0 ){

      ## combine separate dataframes into single one 
      mdf_site <- fluxnet[[ sitename ]]$mdf$inp %>% mutate( mysitename=sitename ) %>%
                                                    left_join( fluxnet[[ sitename ]]$mdf$obs, by = "date" ) %>%
                                                    left_join( fluxnet[[ sitename ]]$mdf[[ outputset ]], by = "date" )

      mdf_fluxnet <- mdf_fluxnet %>% bind_rows( mdf_site )
    
    }
  }

  ## annual
  for (sitename in do.sites_names){
    if ( ncol(fluxnet[[sitename]]$adf[[ outputset ]])>0 ){

      ## combine separate dataframes into single one 
      adf_site <- fluxnet[[ sitename ]]$adf$inp %>% mutate( mysitename=sitename ) %>%
                                                    left_join( fluxnet[[ sitename ]]$adf$obs, by = "date" ) %>%
                                                    left_join( fluxnet[[ sitename ]]$adf[[ outputset ]], by = "date" )

      adf_fluxnet <- adf_fluxnet %>% bind_rows( adf_site )
    
    }
  }

  #---------------------------------------------------------
  # Save complete data to single Rdata file
  #---------------------------------------------------------
  print("writing to files...")

  ## Nested data list
  save( fluxnet, missing_2015, missing_mod, missing_inclim, missing_inevi, file = paste0( datafilnam, ".Rdata") )

  ## Flat data frames
  write_csv( ddf_fluxnet, path = paste0( filn_ddf, ".csv" ) )
  write_csv( wdf_fluxnet, path = paste0( filn_wdf, ".csv" ) )
  write_csv( mdf_fluxnet, path = paste0( filn_mdf, ".csv" ) )
  write_csv( adf_fluxnet, path = paste0( filn_adf, ".csv" ) )

  print("... done saving.")

  # ## Check soil moisture data
  # plot_soilm <- function( ddf_site ){
  #   pl <- ggplot( ddf_site, aes( x=date, y=value, color=variable ) ) +
  #     geom_line( aes( y = wcont, col = "SOFUN s14") ) +
  #     geom_line( aes( y = SWC_F_MDS_1, col="SWC_F_MDS_1")) +
  #     ggtitle( sitename )
  #   plot(pl)
  # }
  # 
  # # for (sitename in unique(ddf_fluxnet$mysitename)){
  # #   plot_soilm( filter( ddf_fluxnet, mysitename==sitename ) )
  # # }
  # 
  # lapply( do.sites_names, function(x) plot_soilm( filter( ddf_fluxnet, mysitename==x ) ) )

  return( list( ddf=ddf_fluxnet, wdf=wdf_fluxnet, mdf=mdf_fluxnet, adf=adf_fluxnet ) )

}
