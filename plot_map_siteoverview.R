plot_map_siteoverview <- function( df, background, plotfiln=NA ){

  require( ncdf4, quietly = TRUE )
  require( fields, quietly = TRUE )
  require( sp, quietly = TRUE )
  require( maptools, quietly = TRUE )
  require( dplyr, quietly = TRUE )  

  source("../utilities/mycolorbar.R")

  ## half degree resolution
  lon <- seq(-179.75, 179.75, 0.5)
  lat <- seq(-89.75, 89.75, 0.5)

  magn <- 4
  ncols <- 2
  nrows <- 1
  widths <- rep(1.4*magn,ncols)
  widths[2] <- 0.17*widths[1]
  heights <- rep(magn,nrows)
  order <- matrix( c(1,2), nrows, ncols, byrow=FALSE)

  ylim <- c(-60,85)
  lat.labels <- seq(-90, 90, 30)
  lat.short  <- seq(-90, 90, 10)
  lon.labels <- seq(-180, 180, 60)
  lon.short  <- seq(-180, 180, 10)

  a <- sapply( lat.labels, function(x) bquote(.(x)*degree ~ N) )
  b <- sapply( lon.labels, function(x) bquote(.(x)*degree ~ E) )

  if (!is.na(plotfiln)) pdf( plotfiln, width=sum(widths), height=sum(heights) )

    panel <- layout(
              order,
              widths=widths,
              heights=heights,
              TRUE
              )
    # layout.show( panel )

    ## Color key
    par( mar=c(3,3,1,1),xaxs="i", yaxs="i",las=1)
    color <- rev( c( "royalblue3", "wheat", "tomato" ))
    lev <- c(0,0.2,0.3,0.4,0.6,0.7,1,1.3,1.6,2,2.5,3)
    out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=FALSE, maxval=150 )

    par( mar=c(3,3,1,1),xaxs="i", yaxs="i",las=1)
    image(
          seq(-179.75, 179.75, 0.5), seq(-89.75, 89.75, 0.5),
          background,
          ylim=c(-60,85),
          # zlim=range(lev),
          yaxt="n", xaxt="n",
          col=out.mycolorbar$colors, breaks=out.mycolorbar$margins
          )
    map( add=TRUE, interior=FALSE, resolution=0, lwd=0.5 )

    axis( 2, at=lat.labels, lab=do.call(expression,a), cex.axis=0.7, lwd=1.5 )
    axis( 2, at=lat.short, lab=F, lwd=1, tck=-0.01 )

    axis( 4, at=lat.labels, lab=F, lwd=1.5 )
    axis( 4, at=lat.short, lab=F, lwd=1, tck=-0.01 )

    axis( 1, at=lon.labels, lab=do.call(expression,b), cex.axis=0.7, lwd=1.5 )
    axis( 1, at=lon.short, lab=F, lwd=1, tck=-0.01 )

    axis( 3, at=lon.labels, lab=F, lwd=1.5 )
    axis( 3, at=lon.short, lab=F, lwd=1, tck=-0.01 )

    # growtype <- list( herb=c("GRA", "CRO"), sav=c("SAV", "WSA"), shrub=c("OSH", "CSH"), woody_dec=c("MF", "DBF"), woody_evg=c("ENF", "EBF"), wet=c("WET") )

    ## Sites used for the analysis
    # with( dplyr::filter( df, group==2 ), points( lon, lat, col='black', pch=19, cex=0.5 ) )
    with( df, points( lon, lat, col='black', pch=21, bg='springgreen2', cex=0.9 ) )

    # ## Sites not used for the analysis
    # with( dplyr::filter( df, !(used) ), points( lon, lat, col='red', pch=4, cex=0.8 ) )    

    # legend( "left", c("Selected sites", "Not selected sites" ), pch=c( 21, 4 ), bty="n", col=c( "black", "red" ), pt.bg=c( "springgreen", "black" ), cex=1.2, box.col="white"  )

    out.mycolorbar <- mycolorbar( color, lev, orient="v", plot=TRUE, maxval=150 )

   
  if (!is.na(plotfiln)) dev.off()

}




 
