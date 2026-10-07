#' Find gradient of line for two points
#'
#' @param point_1 coordinates of first point
#' @param point_2 coordinates of second point
#'
#' @return
#' @export
#'
#' @examples
#' get_gradient()

get_gradient <- function( point_1 = c( 0, 0 ), point_2 = c( 3, 4 ) ){
  
  x_diff <- point_2[1] - point_1[1]
  y_diff <- point_2[2] - point_1[2]
  
  gradient <- y_diff/x_diff
  
  return( gradient )
  
}