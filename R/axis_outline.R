
#' Add lines for axes, quickly, at x=0 and y=0
#'
#' @param col colour of the lines, light grey by default 
#' @param ... other parameters taken by lines, segments, arrows ect...
#'
#' @return
#' @export
#'
#' @examples
#' plot_blank()
#' axis_outline()

axis_outline <- function( col="grey75", ... ){
  
  segments( c( par()$usr[1], 0), 
            c(0, par()$usr[3]), 
            c(par()$usr[2],0), 
            c(0, par()$usr[4]), 
            col=col, ... )
  
}

