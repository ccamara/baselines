
#' Overlay current margins on an active plot, visual query
#'
#' @return overlay on the active plot showing margin region and margin lines, the plot region is shown with min/max data values as dashed rect
#' @export
#'
#' @examples
#' par( mfrow= c(3,3) )
#' plot_blank()
#' overlay_margin()

overlay_margin <- function(){

  # save previous par settings
  prevPar <- par_measures()

  ###___ set colours

  bluePrintColours <- c( "#0C374E", "#2A596F", "#558FA9", "#273A40", "#6E736F", "#B7B2A6", "#EDDECD" )
  tangleColours <- c( '#E63EA0', '#8CC44D', '#F1EECD', '#E9F5EB', '#13222B' )

  colDevRegion <- tangleColours[2]
  colDevLines1 <- tangleColours[4]
  colDevLines2 <- tangleColours[5]
  colDevText <- tangleColours[4]
  alphaDev <- 0.7

  colFigRegion <- tangleColours[3]
  colFigLines1 <- tangleColours[5]
  colFigLines2 <- tangleColours[4]
  colFigText <- tangleColours[5]
  alphaFig <- 0.7

  colPlotRegion <- tangleColours[1]
  colPlotLines1 <- tangleColours[5]
  colPlotLines2 <- tangleColours[4]
  colPlotText <- tangleColours[5]
  alphaPlot <-0.5

  colMarginLine1 <- tangleColours[5]
  colMarginLine2 <- tangleColours[4]
  colMarginText <- tangleColours[5]


  ###___ UNDERLAY for plot region only - highlighted

  #     _________
  #    |  _____  |
  #    | |  _  | |
  #    | | |@| | |
  #    | |_____| |
  #    |_________|

  # blank plot scaled 0 to 1 ... enables easy annotations
  par(new=T)
  plot( 0:1, 0:1, type='n', ann=F, axes=F,
        xaxs=prevPar$par$xaxs,
        yaxs=prevPar$paryaxs,
        xlim=c(1,0) )

  # plot underlay rect to fade out plot region
  rect(  -1, -1, 2, 2,
         col=rgb( 1, 1, 1, 0.5, maxColorValue=1 ),
         border=colPlotRegion, lwd=3  )

  # place colour rect over top
  rect( -1, -1, 2, 2,
        col=adjustcolor( colPlotRegion, alpha.f = alphaPlot )  )

  # dotted region displays the area for xaxs='i' and yaxs='i'
  rect( 0, 0, 1, 1,
        col=F, border = colPlotLines2, lwd=3, lty=3 )


  ###___ FIGURE AREA overlay

  #     _________
  #    |  _____  |
  #    | |@@@@@| |
  #    | |@|_|@| |
  #    | |@@@@@| |
  #    |_________|


  # remove margins for full figure region
  par( mar=c( 0,0,0,0 ) )

  # set figure region
  par( fig= prevPar$par$fig )

  # plot to figure region
  par(new=T)
  plot( 0:1, 0:1, type='n',
        ann=F, axes=F,
        xaxs="i", yaxs="i")

  # create vectors for square in square
  xps <- c( prevPar$par$plt[1], prevPar$par$plt[1], prevPar$par$plt[2], prevPar$par$plt[2], prevPar$par$plt[1],
            0,0,1,1,0 )
  yps <- c( prevPar$par$plt[3], prevPar$par$plt[4], prevPar$par$plt[4], prevPar$par$plt[3], prevPar$par$plt[3],
            0,1,1,0,0 )

  # draw square in square polygon
  polygon(xps, yps,
          col=adjustcolor( colFigRegion, alpha.f = alphaFig) , border=F)


  ###___ plot region only for margins

  #     _________
  #    |  _____  |
  #    | |  _  | |
  #    | | |@| | |
  #    | |_____| |
  #    |_________|

  # reset to plot and figure area
  par( mar = prevPar$par$mar )
  par( fig= prevPar$par$fig )

  # blank plot scaled 0 to 1 ... enables easy annotations
  par(new=T)
  plot( 0:1, 0:1, type='n', ann=F, axes=F,
        xaxs=prevPar$par$xaxs,
        yaxs=prevPar$par$yaxs,
        xlim=c(1,0) )
  ### !!! note x axs is reversed for margin positioning !!!

  ###___ add lines for margins

  # fractions for top and bottom of rect dimensions
  thisStart <- 0
  thisEnd <- 0.1
  thisEnd2 <- 0.2

  # add rects under margin lines
  rect( thisStart, par()$usr[3], thisEnd, -2*prevPar$marginInches[1]/prevPar$plotInches[2],
        col=colPlotLines2, xpd=T, border=F )
  rect( par()$usr[1], thisStart,  par()$usr[1]+2*prevPar$marginInches[2]/prevPar$plotInches[1], thisEnd,
        col=colPlotLines2, xpd=T, border=F )

  rect( thisStart, par()$usr[4], thisEnd, par()$usr[4]+2*prevPar$marginInches[3]/prevPar$plotInches[2],
        col=colPlotLines2, xpd=T, border=F )
  rect( par()$usr[2], thisStart,  -2*prevPar$marginInches[4]/prevPar$plotInches[1], thisEnd,
        col=colPlotLines2, xpd=T, border=F )


  for( i in 1:4 ) {

    for( j in 0:round( prevPar$par$mar[i] , digits=0 ) ) {

      if( j<round( prevPar$par$mar[i] , digits=0 ) ){
        for( k in seq( j, j+1, by=0.2 ) ){
          axis( i, at=c( thisStart, thisEnd ),
                line=k, tck=0, lwd=2, labels=F, col=colPlotLines2, lty=1, xpd=T  )
          axis( i, at=c( thisStart, thisEnd ),
                line=k, tck=0, lwd=0.5, labels=F, col=colPlotLines1, lty=1, xpd=T  )
        }
      }

      axis( i, at=c( thisStart, thisEnd2 ), line=j,
            tck=0, lwd=2, labels=F, col=colPlotLines2, lty=1, xpd=T )
      axis( i, at=c( thisStart, thisEnd2 ), line=j,
            tck=0, lwd=2, labels=F, col=colPlotLines1, lty=1, xpd=T )

      axis( i, at = thisEnd2+ 0.2*(thisEnd2-thisStart),
            line = j-1.2, labels = j,
            tck = 0, lwd = 0, las=1, cex.axis=0.75,
            col = colPlotLines1, lty = 1, xpd = T )

    }
  }


  # reset par settings to previous par
  restore_par( prevPar )


}
