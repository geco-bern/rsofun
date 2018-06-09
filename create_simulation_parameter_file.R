create_simulation_parameter_file <- function(
  path,
  simname,
  sitename,
  firstyeartrend,
  spinupyears,
  nyeartrend,
  recycle,
  daily_out_startyr,
  daily_out_endyr,
  outdt                = 1,
  co2filnam,
  noydepfilnam         = NA,
  nhxdepfilnam         = NA,
  noyfertfilnam        = NA,
  nhxfertfilnam        = NA,
  grharvestfilnam      = NA,
  fapar_forcing_source = NA,
  soilmstress          = FALSE,
  const_nfert_year     = NA,
  const_clim_year      = NA,
  const_lu_year        = NA,
  const_co2_year       = NA,
  const_ndep_year      = NA,
  in_netrad            = FALSE,
  in_ppfd              = FALSE,
  lTrE                 = FALSE,
  lTNE                 = FALSE,
  lTrD                 = FALSE,
  lTND                 = FALSE,
  lGr3                 = FALSE,
  lGN3                 = FALSE,
  lGr4                 = FALSE,
  loutalloc            = FALSE,
  loutplant            = FALSE,
  loutgpp              = FALSE,
  loutnpp              = FALSE,
  loutwaterbal         = FALSE,
  loutdtemp_soil       = FALSE,
  loutdtemp            = FALSE,
  loutdgpp             = FALSE,
  loutdrd              = FALSE,
  loutdtransp          = FALSE,
  loutfapar            = FALSE,
  loutntransform       = FALSE,
  loutnuptake          = FALSE,
  loutlittersom        = FALSE,
  loutturnover         = FALSE,
  loutlanduse          = FALSE,
  loutforcing          = FALSE,
  loutdnpp             = FALSE,
  loutdnup             = FALSE,
  loutdcex             = FALSE,
  loutdCleaf           = FALSE,
  loutdCroot           = FALSE,
  loutdClabl           = FALSE,
  loutdNlabl           = FALSE,
  loutdClitt           = FALSE,
  loutdNlitt           = FALSE,
  loutdCsoil           = FALSE,
  loutdNsoil           = FALSE,
  loutdlai             = FALSE,
  lncoutdtemp          = FALSE,
  lncoutdfapar         = FALSE,
  lncoutdgpp           = FALSE,
  lncoutdwaterbal      = FALSE
  ){


  system( paste0( "cp EXPNAME.sofun.parameter ", path ) )

  ## pattern replace site name
  system( paste0( "sed -i ", systr, " 's/XXXsitenameXXX/", sitename, "/g' ", path ) )

  ## first simulation year (=start of the experiment - 1)
  system( paste0( "sed -i ", systr, " 's/XXXfirstyeartrendXXX/", format( firstyeartrend, digits=4 ), "/g' ", path ) )

  ## number of spinup years
  system( paste0( "sed -i ", systr, " 's/XXXspinupyearsXXX/", as.character( spinupyears ), "/g' ", path ) )

  ## number of simulation years
  system( paste0( "sed -i ", systr, " 's/XXXnyeartrendXXX/", as.character( nyeartrend ), "/g' ", path ) )

  ## climate recycling periodicity for spinup
  system( paste0( "sed -i ", systr, " 's/XXXrecycleXXX/", as.character( recycle ), "/g' ", path ) )

  ## output years for daily output
  system( paste0( "sed -i ", systr, " 's/XXXdaily_out_startyrXXX/", format( daily_out_startyr, digits=4 ), "/g' ", path ) )
  system( paste0( "sed -i ", systr, " 's/XXXdaily_out_endyrXXX/"  , format( daily_out_endyr  , digits=4 )  , "/g' ", path ) )

  # NetCDF output periodicity (d)
  system( paste0( "sed -i ", systr, " 's/XXXoutdtXXX/", as.character( outdt ), "/g' ", path ) )

  ## CO2 file name
  system( paste0( "sed -i ", systr, " 's/XXXco2filnamXXX/", co2filnam, "/g' ", path ) )

  ## constant conditions flags
  if (!is.na(const_nfert_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_nfert_yearXXX/", as.character( const_nfert_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_nfert_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_clim_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_clim_yearXXX/", as.character( const_clim_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_clim_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_lu_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_lu_yearXXX/", as.character( const_lu_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_lu_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_co2_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_co2_yearXXX/", as.character( const_co2_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_co2_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_ndep_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_ndep_yearXXX/", as.character( const_ndep_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_ndep_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }

  ## Additional variables prescribed from data
  if (in_netrad){
    system( paste0( "sed -i ", systr, " 's/XXXin_netradXXX/.true./g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXin_netradXXX/.false./g' ", path ) )  
  }
  if (in_ppfd){
    system( paste0( "sed -i ", systr, " 's/XXXin_ppfdXXX/.true./g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXin_ppfdXXX/.false./g' ", path ) )  
  }
  
  ## Ndep file name
  if (!is.na(noydepfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnoydepfilnamXXX/", noydepfilnam, "/g' ", path ) )
  }
  if (!is.na(nhxdepfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnhxdepfilnamXXX/", nhxdepfilnam, "/g' ", path ) )
  }

  ## Nfert file name
  if (!is.na(noyfertfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnoyfertfilnamXXX/", noyfertfilnam, "/g' ", path ) )
  }
  if (!is.na(nhxfertfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnhxfertfilnamXXX/", nhxfertfilnam, "/g' ", path ) )
  }

  ## fAPAR forcing file name code ('evi_modissubset' or 'modis')
  if (!is.na(fapar_forcing_source)){
    system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/", fapar_forcing_source, "/g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/NA/g' ", path ) )
  }

  ## fAPAR forcing file name code ('evi_modissubset' or 'modis')
  if (!is.na(soilmstress)){
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/", soilmstress, "/g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/NA/g' ", path ) )
  }  

  ## grass harvest file name
  if (!is.na(grharvestfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXgrharvestfilnamXXX/", grharvestfilnam, "/g' ", path ) )
  }

  ## PFTs activated for this simulation
  if (lTrE){
    system( paste( "sed -i ", systr, " 's/XXXlTrEXXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlTrEXXX/.false./g' ", path, sep="" ) )        
  }
  if (lTNE){
    system( paste( "sed -i ", systr, " 's/XXXlTNEXXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlTNEXXX/.false./g' ", path, sep="" ) )        
  }
  if (lTrD){
    system( paste( "sed -i ", systr, " 's/XXXlTrDXXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlTrDXXX/.false./g' ", path, sep="" ) )        
  }
  if (lTND){
    system( paste( "sed -i ", systr, " 's/XXXlTNDXXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlTNDXXX/.false./g' ", path, sep="" ) )        
  }
  if (lGr3){
    system( paste( "sed -i ", systr, " 's/XXXlGr3XXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlGr3XXX/.false./g' ", path, sep="" ) )        
  }
  if (lGN3){
    system( paste( "sed -i ", systr, " 's/XXXlGN3XXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlGN3XXX/.false./g' ", path, sep="" ) )        
  }
  if (lGr4){
    system( paste( "sed -i ", systr, " 's/XXXlGr4XXX/.true./g' ", path, sep="" ) )        
  } else {
    system( paste( "sed -i ", systr, " 's/XXXlGr4XXX/.false./g' ", path, sep="" ) )        
  }

  ## select outputs to be written
  if (loutplant) {
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXX/.false./g' ", path ) )
  }
  if (loutalloc) {
    system( paste0( "sed -i ", systr, " 's/XXXloutallocXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutallocXXX/.false./g' ", path ) )
  }
  if (loutgpp) {
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.false./g' ", path ) )
  }
  if (loutnpp) {
    system( paste0( "sed -i ", systr, " 's/XXXloutnppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutnppXXX/.false./g' ", path ) )
  }
  if (loutwaterbal) {
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.false./g' ", path ) )
  }
  if (loutdtemp_soil){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.false./g' ", path ) )    
  }
  if (loutdtemp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.false./g' ", path ) )    
  }
  if (loutdgpp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdgppXXX/.false./g' ", path ) )
  }
  if (loutdrd){
    system( paste0( "sed -i ", systr, " 's/XXXloutdrdXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdrdXXX/.false./g' ", path ) )
  }
  if (loutdtransp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtranspXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtranspXXX/.false./g' ", path ) )
  }
  if (loutfapar){
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.false./g' ", path ) )
  }
  if (loutntransform){
    system( paste0( "sed -i ", systr, " 's/XXXloutntransformXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutntransformXXX/.false./g' ", path ) )
  }
  if (loutnuptake){
    system( paste0( "sed -i ", systr, " 's/XXXloutnuptakeXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutnuptakeXXX/.false./g' ", path ) )    
  }
  if (loutlanduse){
    system( paste0( "sed -i ", systr, " 's/XXXloutlanduseXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutlanduseXXX/.false./g' ", path ) )    
  }
  if (loutforcing){
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.false./g' ", path ) )    
  }
  if (loutlittersom){
    system( paste0( "sed -i ", systr, " 's/XXXloutlittersomXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutlittersomXXX/.false./g' ", path ) )
  }
  if (loutturnover){
    system( paste0( "sed -i ", systr, " 's/XXXloutturnoverXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutturnoverXXX/.false./g' ", path ) )
  }
  if (loutdnpp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdnppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdnppXXX/.false./g' ", path ) )
  }
  if (loutdClitt){
    system( paste0( "sed -i ", systr, " 's/XXXloutdClittXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdClittXXX/.false./g' ", path ) )
  }
  if (loutdNlabl){
    system( paste0( "sed -i ", systr, " 's/XXXloutdNlablXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdNlablXXX/.false./g' ", path ) )
  }
  if (loutdClabl){
    system( paste0( "sed -i ", systr, " 's/XXXloutdClablXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdClablXXX/.false./g' ", path ) )
  }
  if (loutdCroot){
    system( paste0( "sed -i ", systr, " 's/XXXloutdCrootXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdCrootXXX/.false./g' ", path ) )
  }
  if (loutdCleaf){
    system( paste0( "sed -i ", systr, " 's/XXXloutdCleafXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdCleafXXX/.false./g' ", path ) )
  }
  if (loutdcex){
    system( paste0( "sed -i ", systr, " 's/XXXloutdcexXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdcexXXX/.false./g' ", path ) )
  }
  if (loutdnup){
    system( paste0( "sed -i ", systr, " 's/XXXloutdnupXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdnupXXX/.false./g' ", path ) )
  }
  if (loutdNlitt){
    system( paste0( "sed -i ", systr, " 's/XXXloutdNlittXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdNlittXXX/.false./g' ", path ) )
  }
  if (loutdCsoil){
    system( paste0( "sed -i ", systr, " 's/XXXloutdCsoilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdCsoilXXX/.false./g' ", path ) )
  }
  if (loutdNsoil){
    system( paste0( "sed -i ", systr, " 's/XXXloutdNsoilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdNsoilXXX/.false./g' ", path ) )
  }
  if (loutdlai){
    system( paste0( "sed -i ", systr, " 's/XXXloutdlaiXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdlaiXXX/.false./g' ", path ) )
  }
  if (lncoutdtemp){
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdtempXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdtempXXX/.false./g' ", path ) )
  }
  if (lncoutdfapar){
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdfaparXXX/.false./g' ", path ) )
  }
  if (lncoutdgpp){
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdgppXXX/.false./g' ", path ) )
  }
  if (lncoutdwaterbal){
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdwaterbalXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlncoutdwaterbalXXX/.false./g' ", path ) )
  }

  print( paste0( "finished writing ", path ) )

  return( path )

}