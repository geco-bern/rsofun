calc_soilm <- function( prec, et, method="bucket" ){

  ## Arguments
  ## prec : precipitation (mm d-1)
  ## et   : ecosystem-scale evapotranspiration (mm d-1)

  ## Parameters
  ndayyear <- 365
  nyrspinup <- 1 # number of spinup years
  whc <- 220.0  # water holding capacity (mm)

  ## keenan parameters
  por <- 0.4
  hg  <- 1

  ## orthbucket parameters
  # exp_runoff <- 6.4
  exp_runoff <- 6.4

  nsteps <- length(prec)
  outsoilm <- rep( NA, nsteps )

  ##---------------------------------------------------------
  ## Spinup soil moisture with first 365 days' data  
  ##---------------------------------------------------------
  soilm <- whc 
  for (idx in seq( nyrspinup * ndayyear )){

    useidx <- idx %% ndayyear
    if (useidx==0) {useidx <- 1}

    if (method=="bucket"){
      runoff <- max( 0.0, soilm + prec[idx] - whc )

    } else if (method=="keenan"){
      runoff <- 0.0001 * hg * (1.0 - por) * prec[idx]
      runoff <- runoff + 0.01 * soilm                   # add drainage

    } else if (method=="orthbucket"){

      if (soilm < 0.01 * whc && prec[idx]<0.5*whc){
        runoff <- 0.0
      } else {
        runoff <- ( min( 1.0, ( ( soilm / whc ) ^ exp_runoff ) ) ) * prec[idx]
      }

    }

    soilm <- min( max( 0.0, soilm + prec[idx] - et[idx] - runoff ), whc )

  }

  ##---------------------------------------------------------
  ## Run forward
  ##---------------------------------------------------------
  for (idx in seq(nsteps)){

    if (method=="bucket"){
      runoff <- max( 0.0, soilm + prec[idx] - whc )  
    
    } else if (method=="keenan"){
      runoff <- 0.0001 * hg * (1.0 - por) * prec[idx]
      runoff <- runoff + 0.01 * soilm                   # add drainage

    } else if (method=="orthbucket"){

      if (soilm < 0.01 * whc && prec[idx]<0.5*whc){
        runoff <- 0.0
      } else {
        runoff <- ( min( 1.0, ( ( soilm / whc ) ^ exp_runoff ) ) ) * prec[idx]
      }

    }
 
     soilm <- min( max( 0.0, soilm + prec[idx] - et[idx] - runoff ), whc )
 
    outsoilm[idx] <- soilm 
    
  }

  return( outsoilm )

}