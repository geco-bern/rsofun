analyse_modobs <- function( mod, obs,
                            yintersect0=FALSE, 
                            plot.fil=NA, 
                            xlim=NA, 
                            ylim=NA, 
                            plot.title=NA, 
                            plot.col=NA, 
                            do.plot=TRUE, 
                            plot.linmod=TRUE, 
                            corner="bottomright",
                            lab.xpos=0.75,
                            lab.ypos=0.75,
                            ... ){

  .libPaths( c( .libPaths(), "/home/bstocker/R/x86_64-pc-linux-gnu-library/3.3") )

  syshome <- Sys.getenv( "HOME" )
  source( paste( syshome, "/.Rprofile", sep="" ) )

  require(Metrics)
  require(hydroGOF)
  require(LSD)

  require(hexbin)

  ## get statistics
  idxs <- which(!is.na(mod) & !is.na(obs))
  numb   <- sum(!is.na(obs))
  rmse   <- Metrics::rmse( obs[idxs], mod[idxs] )
  prmse  <- 100 * rmse / mean( obs, na.rm = TRUE )
  if (yintersect0){
    linmod <- lm( mod ~ obs + 0 )
  } else {
    linmod <- lm( mod ~ obs )
  }
  # rsq    <- summary( linmod )$r.squared
  rsq    <- summary( linmod )$adj.r.squared
  nse    <- hydroGOF::NSE( mod, obs, na.rm=TRUE )
  # pbias  <- sum( mod[idxs] - obs[idxs] ) / sum( obs[idxs] )
  pbias  <- mean( (mod[idxs] - obs[idxs]) / obs[idxs] ) 
  ptoohi <- sum( mod[idxs] > obs[idxs] ) / length( idxs )
  # pbias  <- hydroGOF::p( mod, obs, na.rm=TRUE )

  ## plot
  if (do.plot){

    if (!is.na(plot.fil)){
      pdf( plot.fil, width=6, height=6 )      
    }

    par( las=1, mar=c(4.5,4.5,3,2) )

    if (is.na(xlim)) xlim <- c( min(range(mod, na.rm=TRUE)[1], range(obs, na.rm=TRUE)[1]), max(range(mod, na.rm=TRUE)[2], range(obs, na.rm=TRUE)[2]) )
    if (is.na(ylim)) ylim <- xlim

    heatscatter( 
                obs, 
                mod, 
                main="", 
                xlim=xlim, 
                ylim=ylim,
                ...
                )
    abline( c(0,0), c(1,1), col="red" )
    if (plot.linmod) abline( linmod, col="red", lty=2 )

    # mtext( paste( "RMSE =", format( rmse, digits = 3 ) ), side=3, line=0, cex=1.0, adj=0.0 )
    # mtext( bquote( R^2 == .(format( rsq, digits = 3) ) ), side=3, line=1, cex=1.0, adj=0.0 )
    # mtext( paste( "NSE =", format( nse, digits = 3 ) ), side=3, line=2, cex=1.0, adj=0.0 )
    # mtext( paste( "N =", format( sum(!is.na(obs)), digits = 1 ) ), side=3, line=0, cex=1.0, adj=1.0 )  

    if (corner=="bottomright"){
      x0 <- lab.xpos*xlim[2]
      y0 <- seq(1, 5, 1) * (ylim[2]-ylim[1]) * 0.06
    } else if (corner=="topleft"){
      x0 <- 0.05*xlim[2]
      y0 <- lab.ypos*(ylim[2]-ylim[1])+ylim[1]      
    }

    text( x0, y0[1], paste( "bias =", format( pbias, digits = 3 ), "%" ),    adj=0.0, cex=0.8 )
    text( x0, y0[2], paste( "RMSE =", format( rmse, digits = 2 ), " (", format( prmse, digits = 2 ), "%)", sep="" ), adj=0.0, cex=0.8 )
    text( x0, y0[3], bquote( italic(R)^2 == .(format( rsq, digits = 2) ) ),  adj=0.0, cex=0.8 )
    text( x0, y0[4], paste( "NSE =", format( nse, digits = 2 ) ),            adj=0.0, cex=0.8 )
    text( x0, y0[5], paste( "N =", format( numb, digits = 1 ) ), adj=0.0, cex=0.8 )

    title( plot.title, cex.main=0.9, font=1 )
    if (!is.na(plot.fil)){
      dev.off()
    }
  }

  ## return statistics  
  out <- list( rmse=rmse, linmod=linmod, rsq=rsq, nse=nse, prmse=prmse, pbias=pbias, ptoohi=ptoohi, N=numb )
  return( out )
}
