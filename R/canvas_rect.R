#' Draw a rectangle to the canvas/device region (rather than the plot region)
#'
#' @param x1 left coordinate of rectangle
#' @param y1 bottom coordinate
#' @param x2 right coordinate
#' @param y2 top coordinate
#' @param border colour of the border line
#' @param col fill colour of rect
#' @param angle angle (in degrees) of the shading lines.
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

canvas_rect <- function( x1 = 0.1, y1 = 0.5, x2 = 0.9, y2 = 0.9,
                      border=T, col="lemonchiffon", angle=NULL,  ... ){

  # save par settings
  par_to_restore <- par()

  # set plot region to rect area
  par( fig = c( x1, x2, y1, y2 ), new=T )
  #remove margins
  par( mar=c(0,0,0,0), oma=c(0,0,0,0) )

  #ways.blank.plot( x = 1:2, ... )
  plot_blank( xaxs="i", yaxs="i" )

  # add rect to fill plot region and so rect area
  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4],
        border=border, col=col, angle=angle, ...  )

  restore_par( par_to_restore )
  restore_figure_region( par_to_restore )

}
