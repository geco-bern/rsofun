mycolorbar <- function( col,           # a vector of colors from which to interpolate
                       lev,            # levels of colorbar either in the form of c(min,max, N levels) or by a vector of length > 3 containing margins of levels
                       plot=TRUE,      # if false, then no colorbar is plotted but colors, levels and margins are returned
                       alpha=NA,       # transparency value, 0=100% transparent, 1=0% transparent
                       orient="h",     # orentation of colorbar
                       maxval=NA,      # maximum value, overrides upper margin
                       minval=NA,      # minimum value, overrides lower margin 
                       mincol=NA,      # overrides color for lowest level
                       dolabels=TRUE,  # add labels for margins to colorbar
                       doticks=TRUE,   # add tick marks at margins to colorbar
                       cex.axis=1.0,   # magnification of axis tickmarks
                       cex.lab=1.0     # magnification of axis labels
                       ) {
  ## /////////////////////////////////////////////////////////////////////////
  ## Function 'mycolorbar' draws a colorbar based on the image function
  ## 'layout' must be called before to arrange the colorbar in a subplot as
  ## defined by 'layout'.
  ## Beni Stocker, 3.6.2013
  ## -------------------------------------------------------------------------

  if (!requireNamespace("gplots", quietly = TRUE))
    stop("Please, install 'gplots' package")

  if (length(lev)>3){
    explicit <- TRUE
  } else {
    explicit <- FALSE
  }

  if (explicit){

    ## Assume 'lev' declares explicit margins
    # print("MYCOLORBAR: assuming explicit margins provided")

    len <- length(lev)
    # print(paste("len=",len))

    # margins.eff is used for labels at color key and is returned as $margins
    margins.eff <- lev
    # print(paste("length of margins.eff",length(margins.eff)))
    # print(margins.eff)

    # margins defines where margins.eff are to be labelled
    margins <- seq( from=0, to=(len-1), by=1 )
    margins.lab <- margins.eff
    # print(paste("length of margins",length(margins)))
    # print(margins)

    if (!is.na(maxval)) {
      margins.eff[length(margins)] <- maxval
    } 
    if (!is.na(minval)){
      margins.eff[1] <- minval
    }    

    ## Define color key centers (mid-points between margins)
    centers <- seq( from=0.5, to=(len-1.5), by=1 )
    # print(paste("length of centers",length(centers)))
    # print(centers)

    ## Define color range
    colors  <- colorRampPalette( col )( length(centers) )

  } else {
    
    ## Assume 'lev' declares (min,max,number of levels)
    ## Define color key margins
    len <- lev[3]
    margins <- seq( from=lev[1], to=lev[2], by=(lev[2]-lev[1])/len )
    margins.eff <- margins
    margins.lab <- margins
    if (!is.na(maxval)) {
      margins.eff[length(margins)] <- maxval
      margins.lab[length(margins)] <- margins[length(margins)]
    } 
    if (!is.na(minval)){
      margins.eff[1] <- minval
      margins.lab[1] <- margins[1]
    }
    
    ## Define color key centers (mid-points between margins)
    centers <- seq( from=lev[1]+(lev[2]-lev[1])/(2*len), to=lev[2]-(lev[2]-lev[1])/(2*len), by=(lev[2]-lev[1])/len )

    ## Define color range
    colors  <- colorRampPalette( col )( lev[3] )
  
  }

  if (!is.na(mincol)){
    colors[1] <- col2hex(mincol)
  }

  ## Alpha is transparency value
  if (!is.na(alpha)){
    colors <- col2rgb( colors, alpha=TRUE )/255
    colors[4,] <- alpha
    colors <- rgb(colors[1,],colors[2,],colors[3,],colors[4,])
  }

  if (plot) {

    if (dolabels==FALSE) {
      labels=FALSE
    } else {
      if (orient=="h"){
        if (explicit) {
          labels <- as.character(margins.lab)
        } else {
          labels <- as.character(margins.lab)
        }
      } else if (orient=="v") {
        if (explicit) {
          labels <- as.character(margins.lab) 
        } else {
          labels <- as.character(margins.lab)
        }
      } else {
        print("argument 'orient' must be either 'v' for vertical or 'h' for horizontal.")
      }
    }

    if (orient=="h"){
      if (explicit) {
        # xlim <- c(lev[1],lev[len])
        xlim <- c(margins[1],margins[length(margins)])
        image( centers, 0.5, as.matrix(centers), xlim=xlim, col=colors, axes=FALSE, ylab="", xlab="", cex.axis=cex.axis, cex.lab=cex.lab )
        box()
      } else {
        xlim <- c(lev[1],lev[2])
        image( centers, 0.5, as.matrix(centers), col=colors, axes=FALSE, xlim=xlim, ylab="", xlab="", cex.axis=cex.axis, cex.lab=cex.lab )
        box()
      }
      box()
    } else if (orient=="v") {
      if (explicit) {
        ylim <- c(margins[1],margins[length(margins)])
        image( 0.5, centers, as.matrix(t(centers)), col=colors, axes=FALSE, ylim=ylim, xlab="",ylab="", cex.axis=cex.axis, cex.lab=cex.lab )
        box()
        # Hack by substracting 1 in the following 2 lines (commented out original lines above)
        # ylim <- c(lev[1],lev[len])
        # image( 0.5, centers-1, as.matrix(t(centers)), col=colors, axes=FALSE, ylim=ylim, xlab="",ylab="", cex.axis=cex.axis, cex.lab=cex.lab )
        # axis( 2, at=margins-1, labels=as.character(lev) )
        } else {
        ylim <- c(lev[1],lev[2])
        image( 0.5, centers, as.matrix(t(centers)), col=colors, axes=FALSE, ylim=ylim, xlab="",ylab="", cex.axis=cex.axis, cex.lab=cex.lab )
        box()
      }
      box()      
    } else {
      print("argument 'orient' must be either 'v' for vertical or 'h' for horizontal.")
    }

    if (doticks) {
      if (orient=="h"){
        axis( 1, at=margins, labels=labels, cex.axis=cex.axis, cex.lab=cex.lab )
      } else if (orient=="v") {
        axis( 2, at=margins, labels=labels, cex.axis=cex.axis, cex.lab=cex.lab )
      }
    }

  }
  
  out.mycolorbar <- list()
  out.mycolorbar$colors <- colors
  out.mycolorbar$margins <- margins.eff
  out.mycolorbar$centers <- centers
  
  return(out.mycolorbar)

}

