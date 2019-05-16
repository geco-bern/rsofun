#' Analyse modelled values versus observed data.
#'
#' Calculates a set of performance statistics and optionally creates plots of modelled versus observed values.
#'
#' @param mod A vector of numeric values representing modelled values. 
#' @param obs A vector of numeric values representing observed values. 
#' @param heat If \code{TRUE}, uses the heat color palette from LSD. Defaults to \code{FALSE}.
#' @param plot.fil A character string specifying the name of the file containing the plot.
#' @param xlim A vector of length 2 specifying the limits of the x-axis of the plot.
#' @param ylim A vector of length 2 specifying the limits of the y-axis of the plot.
#' @param plot.title A character string specifying the title used for the plot.
#' @param plot.subtitle A character string specifying the subtitle used for the plot.
#' @param do.plot A logical value specifying whether a plot should be created.
#' @param plot.linmod A logical value specifying whether a linear regression should be plotted.
#' @param corner A character string specifying where performance statistics annotations should be placed. Defaults to "bottomright". Any of "bottomright", or "topleft".
#' @param lab.xpos A numeric value specifying the location of the performance statistics annotations in x-direction. Defaults to 0.75.
#' @param lab.ypos A numeric value specifying the location of the performance statistics annotations in y-direction. Defaults to 0.75.
#'
#' @export
#'
#' @examples
#' 
analyse_modobs <- function( mod, 
                            obs,
                            heat = FALSE,
                            plot.fil=NA, 
                            xlim=NA, 
                            ylim=NA, 
                            plot.title=NA,
                            plot.subtitle=NA,
                            do.plot=TRUE, 
                            plot.linmod=TRUE, 
                            corner="bottomright",
                            lab.xpos=0.75,
                            lab.ypos=0.75,
                            main="",
                            log=NA,
                            ... 
                            ){


  ## get statistics
  idxs <- which(!is.na(mod) & !is.na(obs))
  numb   <- sum(!is.na(obs))
  rmse   <- Metrics::rmse( obs[idxs], mod[idxs] )
  prmse  <- 100 * rmse / mean( obs, na.rm = TRUE )
  linmod <- lm( obs ~ mod )
  rsq    <- summary( linmod )$adj.r.squared
  # nse    <- hydroGOF::NSE( obs, mod, na.rm=TRUE )
  pbias  <- mean( (mod[idxs] - obs[idxs]) / obs[idxs] ) 
  bias   <- mean( mod[idxs] - obs[idxs] )
  slope  <- coef(linmod)[2]

  ## plot
  if (do.plot){

    if (!is.na(plot.fil)){
      pdf( plot.fil, width=6, height=6 )      
    }

    par( las=1, mar=c(4.5,4.5,4,2) )

    if (identical(xlim, NA)) xlim <- c( min(range(mod, na.rm=TRUE)[1], range(obs, na.rm=TRUE)[1]), max(range(mod, na.rm=TRUE)[2], range(obs, na.rm=TRUE)[2]) )
    if (identical(ylim, NA)) ylim <- xlim

    if (heat){
      if (!is.na(log)){
        LSD::heatscatter( 
          mod, 
          obs, 
          main=main,
          log=log,
          ...
        )      
      } else {
        LSD::heatscatter( 
          mod, 
          obs, 
          main=main, 
          xlim=xlim, 
          ylim=ylim,
          ...
        )      
      }
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

    title( plot.title, cex.main = 0.9, font = 1 )
    mtext( plot.subtitle, side = 3, line = 0.5, cex = 0.9 )

    if (!is.na(plot.fil)){
      dev.off()
    }
  }

  ## return statistics  
  out <- list( rmse=rmse, linmod=linmod, rsq=rsq, prmse=prmse, bias=bias, slope=slope, N=numb ) # , nse=nse
  return( out )
}
