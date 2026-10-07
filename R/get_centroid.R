#' Calculate centre point (centroid) for a 2d shape
#'
#' @param x coordinate
#' @param y coordinates
#'
#' @return
#' @export
#'
#' @examples


get_centroid <- function( x, y ){
  
  x1 <- sum( x )/length( x )
  y1 <- sum( y )/length( y )
  
  return( cbind(x1, y1 ))
}

