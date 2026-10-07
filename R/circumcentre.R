#' Find circle around triangle (circumcentre and radius)
#'
#' @param point_A x y coordinate of A (length 2) or for A, B & C if dim 3, 2 
#' @param point_B x y coordinate of B (length 2)
#' @param point_C x y coordinate of C (length 2)
#'
#' @return
#' @export
#'
#' @examples
#' circumcentre
#' circle_around_triangle

circumcentre <- circle_around_triangle <- function( point_A=c(-2,1), 
                                                 point_B=c(1,2), 
                                                 point_C=c(-0.5,-3)  ){
      
      # i.e. finds the intersection between two perpendicular bisectors of lines
      # in a triangle, and so must intersect because they cannot be parrallel
      
      if( is.null( dim(point_A))==F){
                  if( dim(point_A)[1] == 3 & dim(point_A)[2]==2 ){
                      points_ABC <- as.data.frame( point_A )
                  } else{
                    message( "if supplying all points via point_A, please check dim is 3,2")
                  }
      }else if ( length( point_A )==2 & length( point_B )==2 & length( point_C )==2 ){
        points_ABC <- as.data.frame( rbind( point_A, point_B, point_C ) )
      }else{
        message( "please check each point 2 values (x, y coordinates) or point_A has 2 columns (x, Y) and 3 rows, one for each point" ) 
      }
      
      mid_AB <- ( points_ABC[1,] + points_ABC[2,] ) /2
      mid_BC <- ( points_ABC[2,] + points_ABC[3,] ) /2
      mid_AC <- ( points_ABC[1,] + points_ABC[3,] ) /2
      
      slope_AB_bi <- -1 / (( points_ABC[2,2] - points_ABC[1,2] ) / ( points_ABC[2,1] - points_ABC[1,1] ))
      slope_BC_bi <- -1 / (( points_ABC[3,2] - points_ABC[2,2] ) / ( points_ABC[3,1] - points_ABC[2,1] ))
      slope_AC_bi <- -1 / (( points_ABC[3,2] - points_ABC[1,2] ) / ( points_ABC[3,1] - points_ABC[1,1] ))
      
      intercept_AB_bi <- mid_AB[2] - ( slope_AB_bi*mid_AB[1] )
      intercept_BC_bi <- mid_BC[2] - ( slope_BC_bi*mid_BC[1] )
      intercept_AC_bi <- mid_AC[2] - ( slope_AC_bi*mid_AC[1] )
      
      circ_X <- ( intercept_BC_bi - intercept_AB_bi) / ( slope_AB_bi - slope_BC_bi)
      circ_Y <- intercept_AB_bi + ( slope_AB_bi*circ_X ) 
      
      circ_X_2 <- ( intercept_AC_bi - intercept_AB_bi) / ( slope_AB_bi - slope_AC_bi)
      circ_Y_2 <- intercept_AB_bi + ( slope_AB_bi*circ_X ) 
      
      
      hyp_AB <- ( ( points_ABC[2,1] - points_ABC[1,1] )^2 + ( points_ABC[2,2] - points_ABC[1,2] )^2 )^0.5
      hyp_BC <- ( ( points_ABC[3,1] - points_ABC[2,1] )^2 + ( points_ABC[3,2] - points_ABC[2,2] )^2 )^0.5
      hyp_AC <- ( ( points_ABC[3,1] - points_ABC[1,1] )^2 + ( points_ABC[3,2] - points_ABC[1,2] )^2 )^0.5
      
      circ_radius <- ( hyp_AB*hyp_BC*hyp_AC ) / ( ( hyp_AB + hyp_BC + hyp_AC )*( -hyp_AB + hyp_BC + hyp_AC )*( hyp_AB - hyp_BC + hyp_AC )*( hyp_AB + hyp_BC - hyp_AC ) )^0.5      
      this_circle <- circ_radius * arc_points()
      
      circumcentre <-  c( circ_X, circ_Y, circ_radius, circ_X_2, circ_Y_2, points_ABC[1,1], points_ABC[1,2], points_ABC[2,1], points_ABC[2,2], points_ABC[3,1], points_ABC[3,2] ) 
      names( circumcentre ) <- c( "x", "y", "radius", "test_X", "test_Y", "AX", "AY", "BX", "BY", "CX", "CY" )
      return( circumcentre)

}

