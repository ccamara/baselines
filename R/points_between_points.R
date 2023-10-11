



#' Create N divisions between two points (straight line)
#'
#' @param point_1 coordinates of first point c( x1 ,y1 )
#' @param point_2 coordinates of second point c( x2, y2 )
#' @param distance value(s) from 0 to 1 for distance(s) between points
#'
#' @return data frame of coordinates for the points, between points
#' @export
#'
#' @examples

points_between_points <- function( point_1, point_2, distance=0.5 ){

  # TO DO
  # ADD checks that 2 values are given for each point, x and y....
  # CHECK for data.frame issues

  point_1_a <- as.numeric( point_1 )
  point_2_a <- as.numeric( point_2 )

  # calc difference in x and y - only for pairs
  x_diff <- point_2_a[1] - point_1_a[1]
  y_diff <- point_2_a[2] - point_1_a[2]

  a_slope <- y_diff / x_diff

  # calculate intercept from x and y of point 2
  #if( a_slope != Inf ){
  if( point_1_a[1] != point_2_a[1] ){
      b_intercept <- point_2_a[2] - a_slope * point_2_a[1]

      # get angle between points - not needed now
      # xy_dist <- ( x_diff^2 + y_diff^2 )^0.5
      # angle_between <- asin( y_diff / xy_dist )

      x_new_points <- point_1_a[1] + ( x_diff  * distance )

      y_new_points <- a_slope * x_new_points + b_intercept
  } else{
      x_new_points <- rep( point_1_a[1], length( distance ) )
      y_new_points <- seq( point_1_a[2] + y_diff * head( distance, n=1 ) ,
                           point_1_a[2] + y_diff * tail( distance, n=1 ), l= length( distance ) )
  }


  new_points <- as.data.frame( cbind( x_new_points, y_new_points ) )

  return( new_points )

}



