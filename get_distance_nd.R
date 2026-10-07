
get_distance_nd <- function( these_points = as.data.frame( cbind( c( 0, 0, 0, 2 ), c( 3, 4, 0, 2 ) ) ) ){

      if( is.data.frame( these_points ) == F || dim( these_points )[2] != 2 ){

            message( " 'these_points' must be a data frame with 2 columns (1 for each point) and 1 row per dimension (i.e. 2 rows gives 2d distance, 3 rows for 3d, n rows gives nd)" )

      } else {

            # these_points <- as.data.frame( cbind( points_1,  points_2 ) )
            # n_dimensions <- dim( these_points )[2]

            xyn_sum <- 0

            for( i in 1: dim( these_points)[1] ){
              xyn_sum <- xyn_sum + ( these_points[ i, 2 ] - these_points[ i, 1 ] )^2
            }

            xyn_distance <- xyn_sum^0.5
            #xyn_distance <- as.data.frame( xyn_distance )

            return( xyn_distance )

      }

}
