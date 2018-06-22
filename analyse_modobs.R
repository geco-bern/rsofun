analyse_modobs <- function( mod, 
                            obs,
                            heat = FALSE,
                            plot.fil=NA, 
                            xlim=NA, 
                            ylim=NA, 
                            plot.title=NA, 
                            do.plot=TRUE, 
                            plot.linmod=TRUE, 
                            corner="bottomright",
                            lab.xpos=0.75,
                            lab.ypos=0.75,
                            main="",
                            ... ){

  require(Metrics)
  require(hydroGOF)
  require(LSD)

  ## get statistics
  idxs <- which(!is.na(mod) & !is.na(obs))
  numb   <- sum(!is.na(obs))
  rmse   <- Metrics::rmse( obs[idxs], mod[idxs] )
  prmse  <- 100 * rmse / mean( obs, na.rm = TRUE )
  linmod <- lm( obs ~ mod )
  rsq    <- summary( linmod )$adj.r.squared
  nse    <- hydroGOF::NSE( obs, mod, na.rm=TRUE )
  pbias  <- mean( (mod[idxs] - obs[idxs]) / obs[idxs] ) 
  bias   <- mean( mod[idxs] - obs[idxs] )
  slope  <- coef(linmod)[2]

  ## plot
  if (do.plot){

    if (!is.na(plot.fil)){
      pdf( plot.fil, width=6, height=6 )      
    }

    par( las=1, mar=c(4.5,4.5,4,2) )

    if (is.na(xlim)) xlim <- c( min(range(mod, na.rm=TRUE)[1], range(obs, na.rm=TRUE)[1]), max(range(mod, na.rm=TRUE)[2], range(obs, na.rm=TRUE)[2]) )
    if (is.na(ylim)) ylim <- xlim

    if (heat){
      heatscatter( 
                  mod, 
                  obs, 
                  main=main, 
                  xlim=xlim, 
                  ylim=ylim,
                  ...
                  )      
    } else {
      plot( mod, obs, main = main, pch = 16, xlim = xlim, ylim = ylim, ... )
    }
    abline( c(0,0), c(1,1), col="black", lty=3 )
    if (plot.linmod) abline( linmod, col="red", lty=1 )

    ## left
    mtext( bquote( italic(R)^2 == .(format( rsq, digits = 3) ) ), side=3, line=1, cex=1.0, adj=0.0 )
    mtext( paste( "RMSE =", format( rmse, digits = 3 ) ), side=3, line=0, cex=1.0, adj=0.0 )

    ## right
    mtext( paste( "bias =",  format( bias, digits = 3 ) ), side=3, line=2, cex=1.0, adj=1.0 )
    mtext( paste( "slope =", format( slope, digits = 3 ) ), side=3, line=1, cex=1.0, adj=1.0 )
    mtext( bquote( italic(N) == .(format( sum(!is.na(obs)), digits = 3) ) ), side=3, line=0, cex=1.0, adj=1.0 )

    if (corner=="bottomright"){
      x0 <- lab.xpos*xlim[2]
      y0 <- seq(1, 5, 1) * (ylim[2]-ylim[1]) * 0.06
    } else if (corner=="topleft"){
      x0 <- 0.05*xlim[2]
      y0 <- lab.ypos*(ylim[2]-ylim[1])+ylim[1]      
    }

    # text( x0, y0[1], paste( "bias =", format( bias, digits = 3 ), "%" ),    adj=0.0, cex=0.8 )
    # text( x0, y0[2], paste( "RMSE =", format( rmse, digits = 2 ), " (", format( prmse, digits = 2 ), "%)", sep="" ), adj=0.0, cex=0.8 )
    # text( x0, y0[3], bquote( italic(R)^2 == .(format( rsq, digits = 2) ) ),  adj=0.0, cex=0.8 )
    # text( x0, y0[5], paste( "N =", format( numb, digits = 1 ) ), adj=0.0, cex=0.8 )

    title( plot.title, cex.main=0.9, font=1 )
    if (!is.na(plot.fil)){
      dev.off()
    }
  }

  ## return statistics  
  out <- list( rmse=rmse, linmod=linmod, rsq=rsq, nse=nse, prmse=prmse, pbias=pbias, N=numb )
  return( out )
}
