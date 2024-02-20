#' Draw box around figure region
#'
#' @param border colour of border line
#' @param lwd line width of border line
#' @param lty line type of border line
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

box_outer <- function( border="hotpink", lwd=2, lty="33", ... ){

  # save par settings
  beforePar <- par()

  # set to plot area
  par( fig= c( beforePar$fig[1], beforePar$fig[2],
               beforePar$fig[3], beforePar$fig[4] ), new=T )

  #remove margins
  #restore_par()
  par( mar=c(0,0,0,0), oma=c(0,0,0,0) )

  plot( 0:1, 0:1, type='n', axes=F, ann=F, xaxs='i', yaxs='i' )

  # add rect around region
  rect( 0, 0, 1, 1, lty=lty, lwd=lwd, border=border, ...  )

  restore_par( beforePar )
  #RestoreFigureRegion( beforePar )

}
