#' Convert radians to degrees
#'
#' @param radians the values to be converted
#'
#' @return
#' @export
#'
#' @examples

radians_2_degrees <- function( radians ){
  degrees <-  360*radians/(2*pi)
  return( degrees )
}
