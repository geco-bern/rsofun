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
  # fapar_forcing_source = NA,
  soilmstress          = FALSE,
  tempstress           = FALSE,
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
  loutplant            = FALSE,
  loutgpp              = FALSE,
  loutwaterbal         = FALSE,
  loutforcing          = FALSE,
  loutdgpp             = FALSE,
  loutdrd              = FALSE,
  loutdtransp          = FALSE,
  loutdwcont           = FALSE,
  loutdaet             = FALSE,
  loutdpet             = FALSE,
  loutdalpha           = FALSE,
  loutdtemp            = FALSE,
  loutdfapar           = FALSE,
  loutdtemp_soil       = FALSE,
  lcalibgpp            = FALSE,
  lcalibtransp         = FALSE,
  lcalibfapar          = FALSE
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

  # ## fAPAR forcing file name code ('evi_modissubset' or 'modis')
  # if (!is.na(fapar_forcing_source)){
  #   system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/", fapar_forcing_source, "/g' ", path ) )
  # } else {
  #   system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/NA/g' ", path ) )
  # }

  ## switch for soil moisture stress function
  if (soilmstress){
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/.false./g' ", path ) )
  }  

  ## switch for temperature stress function
  if (tempstress){
    system( paste0( "sed -i ", systr, " 's/XXXtempstressXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXtempstressXXX/.false./g' ", path ) )
  }  

  ## grass harvest file name
  if (!is.na(grharvestfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXgrharvestfilnamXXX/", grharvestfilnam, "/g' ", path ) )
  }

  ## PFTs activated for this simulation
  if (lTrE){
    system( paste0( "sed -i ", systr, " 's/XXXlTrEXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTrEXXX/.false./g' ", path ) )        
  }
  if (lTNE){
    system( paste0( "sed -i ", systr, " 's/XXXlTNEXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTNEXXX/.false./g' ", path ) )        
  }
  if (lTrD){
    system( paste0( "sed -i ", systr, " 's/XXXlTrDXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTrDXXX/.false./g' ", path ) )        
  }
  if (lTND){
    system( paste0( "sed -i ", systr, " 's/XXXlTNDXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTNDXXX/.false./g' ", path ) )        
  }
  if (lGr3){
    system( paste0( "sed -i ", systr, " 's/XXXlGr3XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGr3XXX/.false./g' ", path ) )        
  }
  if (lGN3){
    system( paste0( "sed -i ", systr, " 's/XXXlGN3XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGN3XXX/.false./g' ", path ) )        
  }
  if (lGr4){
    system( paste0( "sed -i ", systr, " 's/XXXlGr4XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGr4XXX/.false./g' ", path ) )        
  }

  ## select outputs to be written
  if (loutplant){
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXXilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXX/.false./g' ", path ) )
  }
  if (loutgpp){
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.false./g' ", path ) )
  }
  if (loutwaterbal){
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.false./g' ", path ) )
  }
  if (loutforcing){
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.false./g' ", path ) )
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
  if (loutdwcont){
    system( paste0( "sed -i ", systr, " 's/XXXloutdwcontXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdwcontXXX/.false./g' ", path ) )
  }
  if (loutdaet){
    system( paste0( "sed -i ", systr, " 's/XXXloutdaetXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdaetXXX/.false./g' ", path ) )
  }
  if (loutdpet){
    system( paste0( "sed -i ", systr, " 's/XXXloutdpetXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdpetXXX/.false./g' ", path ) )
  }
  if (loutdalpha){
    system( paste0( "sed -i ", systr, " 's/XXXloutdalphaXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdalphaXXX/.false./g' ", path ) )
  }
  if (loutdtemp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.false./g' ", path ) )
  }
  if (loutdfapar){
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.false./g' ", path ) )
  }
  if (loutdtemp_soil){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.false./g' ", path ) )
  }

  if (lcalibgpp){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibgppXXX/.false./g' ", path ) )
  }
  if (lcalibfapar){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibfaparXXX/.false./g' ", path ) )
  }
  if (lcalibtransp){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibtranspXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibtranspXXX/.false./g' ", path ) )
  }


  # print( paste0( "finished writing ", path ) )

  return( path )

}