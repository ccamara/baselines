#' Retrieve centre point of plot
#'
#' @param x_prop the x position relative to x1 (0) and x2 (1)  
#' @param y_prop the x position relative to x1 (0) and x2 (1) 
#'
#' @return
#' @export
#'
#' @examples

get_plot_centre <- function( x_prop=0.5, y_prop=0.5 ){
  
  x <- par()$usr[1] + x_prop*( par()$usr[2] - par()$usr[1] )
  y <- par()$usr[3] + y_prop*( par()$usr[4] - par()$usr[3] )
  
  return(  c( x, y ) )
  
}