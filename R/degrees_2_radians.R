
#' Convert degrees to radians
#'
#' @param degrees the values to be converted
#'
#' @return
#' @export
#'
#' @examples

degrees_2_radians <- function( degrees ){
  radians <-  (2*pi)*degrees/360
  return( radians )
}

