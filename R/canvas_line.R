#' Draw line over the canvas (device) region as coordiantes in ranges 0 < x < 1, 0 < y < 1
#'
#' @param x1 coordinate of x start point
#' @param y1 coordinate of y start point
#' @param x2 coordinate of x end point
#' @param y2 coordinate of y end point
#' @param col colour of line
#' @param lwd line width
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

canvas_line <- function( x1 = 0.1, y1 = 0.1, x2 = 0.7, y2 = 0.3,
                      col="lemonchiffon3", lwd=2, ... ){

  # save par settings
  beforePar <- par()

  # set plot region to rect area
  par( fig = c( 0, 1, 0, 1 ), new=T )
  #remove margins
  par( mar=c( 0, 0, 0, 0 ), oma=c( 0, 0, 0, 0) )

  #ways.blank.plot( )
  plot_blank( xaxs="i", yaxs="i" )

  # add segments line
  segments( x1, y1, x2, y2,
            col=col, lwd=lwd, ... )

  # reset par to previous
  restore_par( beforePar )
  restore_figure_region( beforePar )

}
