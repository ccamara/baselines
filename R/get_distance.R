
get_distance <- function( point_1 = c( 0, 0 ), point_2 = c( 3, 4 ) ){

  xy_distance <- ( ( point_2[ 1 ]-point_1[ 1 ] )^2 +
                   ( point_2[ 2 ]-point_1[ 2 ] )^2 )^0.5

  return( xy_distance )

}
