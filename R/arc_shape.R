#' Create coordinates for an arc/donut shape with outer and inner radii
#'
#' @param arc_length length of the arc in the units specified
#' @param from position the arc starts from
#' @param units degrees (default) or radians, the units of the arc
#' @param n_points number of points in the  arc
#' @param radius_outer radius of outer arc, should be > inner
#' @param radius_inner inner arc radius, should be < outer
#'
#' @return data frame of shape coordinates
#' @export
#'
#' @examples

arc_shape <- function( arc_length = 360,
                        from = 0,
                        units = "degrees",
                        n_points = 180,
                        radius_outer = 1,
                        radius_inner = 0.5 ){

  if( arc_length >= 360 ) arc_length <- 360
  if( arc_length <= -360 ) arc_length <- -360

  if( radius_inner < radius_outer ){
      radius_outside <- radius_outer
      radius_inside <- radius_inner
  } else if( radius_inner == radius_outer){
      radius_outside <- radius_outer
      radius_inside <- radius_inner
      warning( "radius_outer = radius_inner, line produced rather than shape, consider using arc_points()" )
  } else if( radius_inner > radius_outer){
      radius_outside <- radius_inner
      radius_inside <- radius_outer
      warning( "radius_outer < radius_inner, values switched so outer is inner & vice versa" )
  }

  my_arc <- arc_points( arc_length = arc_length, from = from,
                        units=units, n_points = n_points )

  arc_outside <- radius_outside * my_arc
  arc_inside <- radius_inside * my_arc[ length(my_arc$x):1, ]

  # reduce to circle if one radius is zero
  if( radius_outside == 0 ){
    my_shape <- as.data.frame( rbind( arc_inside, c(0,0) )  )
    warnings( "radius_outer = 0, sector returned rather than segments" )
  } else if ( radius_inside == 0 ) {
    my_shape <- as.data.frame( rbind( arc_outside, c(0,0) )   )
    warnings( "radius_inner = 0, sector returned rather than segments" )
  } else {
    my_shape <- as.data.frame( rbind( arc_outside, arc_inside ) )
  }

  names( my_shape ) <- c( "x", "y" )

  # output
  return( my_shape )


}
