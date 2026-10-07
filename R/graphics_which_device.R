#' quick query to graphics device settings
#'
#' @return
#' @export
#'
#' @examples

graphics_which_device <- function( ){

  current_graphics_device <-  options( "device" )
  return( current_graphics_device )

}
