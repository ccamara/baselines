
# create arc ( circle by default )

arc_points <- function( arc_length = 360,
                       from = 0,
                       units = "degrees",
                       n_points = 180
                       #clockwise=T
                       ){

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
