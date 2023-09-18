#' Create coordinates for an arc
#'
#' @param arc_length length of the arc in the units specified
#' @param from position the arc starts from
#' @param units degrees (default) or radians, the units of the arc
#' @param n_points number of points in the  arc
#'
#' @return a data frame of coordinates for the arc (x, Y)
#' @export
#'
#' @examples
#' arc_points() # returns a circle of 180 points, clockwise from north scaled -1 < x < 1, -1 < y <1
#' plot( arc_points( arc_length = 270, from =90, n_points = 10 ), type='l') # arc of 10 points, clockwise from east to north

arc_points <- function( arc_length = 360,
                       from = 0,
                       units = "degrees",
                       n_points = 180 ){

  # convert to radians if degrees
  if( units == "degrees" ) {
    startRadians <- degrees_2_radians( from )
    endRadians <- degrees_2_radians( arc_length )
  }

  # set direction
  if( endRadians >=0 ) arcDirection <- +1 else arcDirection <- -1

  # angle for last position
  lastRadians <- startRadians + endRadians

  # add warning message if end pos is >1 rotation away

  # make circle res proportional to arc res
  #circleRes <- 1 + ( n_points-1 )*( 2*pi /endRadians )

  # create sequence for sin and cos functions
  pointsOnCircle <-  seq( startRadians, lastRadians, l=n_points )

  # create arc  points
  xArc <- sin( pointsOnCircle )
  yArc <- cos( pointsOnCircle )

  arcPoints <-  as.data.frame( cbind( xArc, yArc ) )
  names( arcPoints ) <- c( "x", "y" )

  # output
  return( arcPoints )

}
