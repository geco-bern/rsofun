## Function returns a data frame (tibble) containing observational data for one 
## variable to be used as target variables for calibration for a given site, 
## covering specified dates.
get_obs_calib_var <- function( var, datasource, sitename, date_start, date_end, varname, datename ){

  require(readr)
  require(dplyr)
  require(lubridate)

  ## look for data for this site in the given directory
  filelist <- list.files(datasource)

  ## read data and change variable names to conform and filter required dates 
  if (grepl("gpp", var) && grepl("FLUXNET-2015_Tier1", datasource) ){
    
    print("Assuming FLUXNET 2015 standard.")

    idx <- lapply(filelist, function(x) grepl(sitename, x)) %>% unlist %>% which()
    filelist <- filelist[idx]
    if (length(idx)>1) {
      idx <- lapply(filelist, function(x) grepl("FULLSET", x)) %>% unlist %>% which()
    }
    filelist <- filelist[idx]
    if (length(idx)>1) {
      idx <- lapply(filelist, function(x) grepl("3.csv", x)) %>% unlist %>% which()
    } else {
      idx <- 1
    }
    filn <- filelist[idx]

    ## slightly different treatment of FLUXNET (standard FLUXNET 2015) data
    df_byvar <- try(read_csv( paste0( datasource, filn ), na="-9999", col_types = cols() ))
    if (class(df_byvar)=="try-error"){
      df_byvar <- tibble( date=NA, var=NA )  
    } else {
      df_byvar <- df_byvar %>%  rename_( var = varname, date = datename ) %>% 
                                mutate( date = ymd( date ) ) %>%
                                select( date, var ) %>% 
                                filter( date >= date_start & date <= date_end )
    }


  } else {

    ## determine file to be read
    idx <- lapply(filelist, function(x) grepl(sitename, x)) %>% unlist %>% which()
    if (length(idx)>1) {idx <- idx[1]; print( paste0("Using only first found file in ", datasource, "for site ", sitename) )}
    filn <- filelist[idx]
    
    ## other data sources  
    df_byvar <- try( read_csv( paste0( datasource, filn ) ) )
    if (class(df_byvar)=="try-error"){
      df_byvar <- tibble( date=NA, var=NA )  
    } else {
      df_byvar <- df_byvar %>%  rename_( var = varname, date = datename ) %>% 
                                select( date, var ) %>% 
                                filter( date >= date_start & date <= date_end )
    }
  
  }

  ## change column names to what it's specified
  colnames(df_byvar) <- c("date", var)
  
  return( df_byvar )

}


## Function returns a data frame (tibble) containing all the observational data
## used as target variables for calibration for a given site, covering specified dates.
get_obs_calib <- function( sitename, targetvars, datasource, date_start, date_end, varnames, datenames ){

  require(plyr)
  
  ## loop over variables to get a data frame for each variable 
  byvar <- lapply( as.list(targetvars), function(x) get_obs_calib_var( x, 
                                          datasource = datasource[[ x ]], 
                                          sitename   = sitename, 
                                          date_start = date_start, 
                                          date_end   = date_end, 
                                          varname    = varnames[[ x ]], 
                                          datename   = datenames[[ x ]] 
                                          ) )

  ## combine variable-specific dataframes along columns
  bysite <- join_all( byvar, by="date" )

  return(bysite)

}

get_obs_raw <- function( settings_calib, settings_sims ){

  require(readr)
  require(dplyr)

  ##------------------------------------------------------------
  ## Read raw observational data from files.
  ## This creates a data frame (tibble) that contains
  ## a column 'date' and columns for each target variable. The number of rows
  ## corresponds to each simulation's length (number of days).
  ##------------------------------------------------------------
  ## loop over sites to get data frame with all variables
  list_bysite <- lapply( settings_sims$sitenames[1:3], function(x) get_obs_calib(x, 
                                                        targetvars = settings_calib$targetvars, 
                                                        datasource = settings_calib$datasource,
                                                        date_start = settings_sims$date_start[[ x ]],
                                                        date_end   = settings_sims$date_end[[ x ]],
                                                        varnames   = settings_calib$varnames,
                                                        datenames  = settings_calib$datenames
                                                        ) %>% mutate( sitename=x ) )
                
  ## combine dataframes from multiple sites along rows
  obs_raw <- bind_rows( list_bysite )

  return( obs_raw )

}


calib_sofun <- function( setup, settings_calib, settings_sims ){


  ## Collect raw observational data
  obs_raw <- get_obs_raw( settings_calib, settings_sims )

  ## Clean observational raw data for variables GPP
  obs <- clean_obs_raw_gpp_fluxnet2015( obs_raw )


}
