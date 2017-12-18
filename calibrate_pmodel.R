
require(ncdf4)

source("analyse_modobs.R")

## Load the data collected by 'get_modobs.R'
load( "df_modobs_fluxnet2015_s14_with_SWC_v4.Rdata" )  # loads df_fluxnet
ndays_0 <- nrow( df_fluxnet )
print( paste( "total number of days, unfiltered: ", ndays_0 ) )

## Remove cold days
df_fluxnet <- df_fluxnet %>% filter( temp > 5.0 )
ndays_1 <- nrow( df_fluxnet )
print( paste0( "total number of days after removing cold days: ", ndays_1, " (", format( ndays_1/ndays_0*100, digits=2 ), "% remaining)") )

## Remove very dry days
df_fluxnet <- df_fluxnet %>% filter( soilm_mean > 0.5 )
ndays_2 <- nrow( df_fluxnet )
print( paste0( "total number of days after removing dry days: ", ndays_2, " (", format( ndays_2/ndays_0*100, digits=2 ), "% remaining)") )

## Plot observed vs. modelled
stats <- with( df_fluxnet, analyse_modobs( GPP_NT_VUT_REF, gpp, yintersect0=TRUE, xlab="modelled", ylab="observed") )

## Optimal correction factor for quantum yield efficiency parameter:
phiO_corr <- stats$linmod$coefficients

## Read GPP NC output to get parameter value of quantum yield efficiency
iset <- "s14"
runname <- "FR-Pue"
dirn <- paste( myhome, "sofun/output_nc_fluxnet2015_sofun/", iset, "/", sep="" )
filn <- paste0( runname, ".d.gpp.nc" )
path       <- paste0( dirn, filn )
nc         <- nc_open( path )
q0_param_used   <- ncatt_get( nc, varid=0, attname="param_kphio_GrC3" )$value %>% as.numeric()
nc_close(nc)

## Calculate optimal apparent quantum yield efficiency parameter:
q0_param_opt <- q0_param_used * phiO_corr

print("------------------------------------------------------------")
print( paste( "Optimal value apparent quantum yield efficiency is: ", format( q0_param_used * phiO_corr, digits = 3 ) ) )
print("------------------------------------------------------------")

# hb <- hexbin( df_fluxnet$GPP_NT_VUT_REF, df_fluxnet$gpp)
# plot( hb )
