#' create a rect with rounded corners
#'
#' @param x1 left x coordinate (defaults to -1 to match arc_points data range)
#' @param y1 bottom y coordinate (defaults to -1 to match arc_points data range)
#' @param x2 right x coordinate (defaults to 1 to match arc_points data range)
#' @param y2 top y coordinate (defaults to 1 to match arc_points data range)
#' @param rounding similarity to circle, 1 = circle & 0 = rect
#' @param n_points number of points in circle, or ceiling( n_points/4 ) for rounded corner
#'
#' @return
#' @export
#'
#' @examples

rect_rounded <- function( x1 = -1, y1 = -1, x2 = 1, y2 = 1, rounding = 0.5, n_points=180 ){

  my_shape <- NULL
  my_rect_rounded <- NULL

  if( rounding < 0 | rounding >1 ) warning( "Rounding outside range  0 <= r <= 1, unusual values produce unusual results" )


  if( rounding == 1 ){

        my_shape <- ( arc_points( arc_length = 360, from = 180, n_points = n_points ) + 1 ) / 2

  }else{

        quarter_points <- ceiling( 0.25* ( n_points ) )

        my_arc <- rounding* ( arc_points( arc_length = 360, from = 180, n_points = 4*quarter_points ) + 1 ) / 2

        my_shape$x <- c( 1-0.5*rounding, 0.5*rounding,
                         my_arc$x[1:quarter_points],
                         0, 0,
                         my_arc$x[ ( quarter_points + 1 ): ( 2*quarter_points ) ],
                         0.5*rounding, 1-0.5*rounding,
                         (1-rounding) +  my_arc$x[ ( 2*quarter_points + 1 ): ( 3*quarter_points) ],
                         1, 1,
                         (1-rounding) +  my_arc$x[ ( 3*quarter_points + 1 ): ( 4*quarter_points )],
                         1-0.5*rounding )

        my_shape$y <- c( 0, 0,
                         my_arc$y[1:quarter_points],
                         0.5*rounding, 1-0.5*rounding,
                         (1-rounding) + my_arc$y[ ( quarter_points + 1 ): ( 2*quarter_points ) ],
                         1, 1,
                         (1-rounding) +  my_arc$y[ ( 2*quarter_points + 1 ): ( 3*quarter_points )],
                         1-0.5*rounding, 0.5*rounding,
                         my_arc$y[ ( 3*quarter_points + 1 ): ( 4*quarter_points )],
                         0 )

  }

  my_rect_rounded$x <- x1 + ( x2 - x1 ) * my_shape$x
  my_rect_rounded$y <- y1 + ( y2 - y1 ) * my_shape$y

  return( my_rect_rounded )

}


