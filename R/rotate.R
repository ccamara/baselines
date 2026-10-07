

#' Title 2d rotate transformation for points, lines and shapes
#'
#' @param x the point or shape coordinates
#' @param y the point or shape coordinates
#' @param centre centre/fixed point of rotation, defaults to origin c(0,0), NUll uses shape's centre
#' @param rotation the degree of rotation, default is in degrees, negative and multiple turns permitted
#' @param units degrees (default) or radians, units of arc
#'
#' @return
#' @export
#'
#' @examples
#'
#' x <- c( 0, 2, 6, 4, 0 )
#' y <- c( 3, 5, 3, 1, 3 )
#' hyp <- ( x^2 + y^2 )^0.5
#' my_shape <- as.data.frame( cbind( x, y ) )
#' this_angle <- 135
#' plot_blank( -max(hyp):max( hyp ), -max(hyp):max( hyp ), asp=1 )
#' for( i in 1:length(x) ) lines( hyp[i]*arc_points(), col="grey70", lwd=0.8 )
#' polygon( my_shape, lwd=2 )
#' xy2 <- rotate( x = my_shape$x, y= my_shape$y,
#'                rotation = this_angle )
#'                polygon( xy2, lwd=2, border="hotpink2" )
#'                segments( x, y, xy2$x2, xy2$y2, lty="44", col="hotpink2" )
#' fixed_p <- c( 3, 3 )
#' points( fixed_p[1], fixed_p[2], pch=19)
#' xy2 <- rotate( x = my_shape$x, y= my_shape$y,
#'                rotation = this_angle, centre = fixed_p  )
#' for( i in 1:length(x) ) lines( x = fixed_p[1] + xy2$hypotenuse[i]*arc_points()[,1],
#'                                y= fixed_p[2] + xy2$hypotenuse[i]*arc_points()[,2],
#'                                col="grey50", lwd=0.8 )
#' polygon( xy2, lwd=2, border="cyan3" )
#' segments( x, y, xy2$x2, xy2$y2, lty="44", col="cyan3" )
#'

rotate <-  function( x, y, centre = c( 0, 0 ), rotation = 0, units = "degrees" ){

  if( is.null(centre) == T ){
      centre <- get_centroid( x, y )
  } else{
      if ( is.numeric( centre ) !=T ) centre <- as.numeric( centre )
  }

  if( units =="radians" ){
      # convert to degrees for turn correction
      deg_con <- radians_2_degrees( rotation )
      # correct for negative rotations and multiple turns
      # convert back to radians for internal triognometry calculations
      rotate_by <- degrees_2_radians( deg_con %% 360 )
  } else if( units == "degrees" ) {
      # correct for negative rotations and multiple turns
      # convert to radians for internal triognometry calculations
      rotate_by <- degrees_2_radians( rotation %% 360 )
  } else {
    # ERROR
    message( "units must be 'degrees' (default) or 'radians'..." )
  }

  # rescale for centre/fixed point
  x1 <- x - centre[ 1 ]
  y1 <- y - centre[ 2 ]

  # calc
  hypotenuse <- ( x1^2 + y1^2 )^0.5

  x2 <- ( x1*cos( -rotate_by ) - y1*sin ( -rotate_by ) )
  y2 <- ( y1*cos( -rotate_by ) + x1*sin ( -rotate_by ) )

  # include pi/2 (90 degree) correction so rotations start at 12 o'clock/North
  if( units=="radians" ){
    start_angle <- atan2( -y1, x1 ) + pi/2
    end_angle <- start_angle + rotation
  } else if( units=="degrees"){
    start_angle <- radians_2_degrees( atan2( -y1, x1 ) + pi/2 )
    end_angle <-  start_angle + rotation
  }

  x2 <- centre[ 1 ] + x2
  y2 <- centre[ 2 ] + y2

  x2y2 <- as.data.frame( cbind( x2,
                                y2,
                                hypotenuse,
                                start_angle,
                                end_angle,
                                rep( centre[1], length(x) ) ,
                                rep( centre[2], length(x) ) ,
                                rep( rotation, length(x) ) ,
                                x,
                                y ) )
  names( x2y2 ) <- c("x2", "y2", "hypotenuse", "start_angle", "end_angle",
                     "centre_x", "centre_y", "rotation_angle", "x1", "y1"  )
  return( x2y2 )

}
