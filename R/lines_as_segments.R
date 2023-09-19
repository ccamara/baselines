
#' Draw lines piecewise, to add widths or colours for each segment
#'
#' @param x values for the x coordinates
#' @param y values for the y coordinates
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples
#' myPoints <- arc_points( arc_length = 180, from = 270, n_points = 10 )
#' plot( myPoints, type='l')
#' lines_as_segments( myPoints$x, myPoints$y, col= rainbow(9), lwd=seq( 3, 9, l=9) )
#' library( HistData )
#' plot( Minard.troops$long, Minard.troops$lat, type='n' )
#' lines_as_segments( Minard.troops$long, Minard.troops$lat,
#'                   lwd=30*Minard.troops$survivors/max(Minard.troops$survivors),
#'                   col=hsv(1, 0.5, 0.4+0.5*( 1-Minard.troops$survivors/max(Minard.troops$survivors)) ) )

lines_as_segments <- function( x, y,
                               #z,
                               ... ){

  segments( x[ 1:(length(x)-1) ], y[ 1:(length(y)-1) ],
            x[ 2:length(y) ], y[ 2:length(y) ], ...  )

}


