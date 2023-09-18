
#' Generate points in a phyllotaxis pattern for visual demonstrations
#'
#' @param n_points number of points to create
#' @param ma value controlling the spread of the pattern
#'
#' @return a data frame of values for each point
#' @export
#'
#' @examples

phyllotaxis_points <- function( n_points=1000, ma ){

# see coding train https://www.youtube.com/watch?v=KWoJgHFYWxY

  c=2
# tightness of packing
ma <- 137.5 * pi/180
n <- 1:n_points

peasA <-  seq( 0.5*pi, 2.5*pi, l=n_points)

angy <- n * ma
r <- c * n^0.6

x <- r * cos( angy )
y <-  r * sin( angy )

# rescale to 0:1

x1 <- x - min( x )
x2 <- x1 / max( x1 )

y1 <- y - min( y )
y2 <- y1 / max( y1 )

phyllotaxis_xy <-  as.data.frame( cbind( x2, y2 ) )

names( phyllotaxis_xy ) <- c( "x", "y" )

# output
return( phyllotaxis_xy )

}



