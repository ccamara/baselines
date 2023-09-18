
#' Draw square brackets, lines, box and other side combinations around rect
#'
#' @param x1 value for left, horizontal position in plot region coordinates
#' @param y1 value for bottom, vertical position in plot region coordinates
#' @param x2 value for right
#' @param y2 value for top
#' @param sides  the sides to be drawn in the form c( 1, 2, 3, 4 ), 1:3 etc.
#' @param col fill colour of the rect
#' @param border border line colour
#' @param ... global par() parameters
#'
#' @return draws a rect with teh specified sides as lines
#' @export
#'
#' @examples

rect_sides <- function( x1, y1, x2, y2, sides=c(1,2,3,4),
                        col=NULL, border="black", ... ){

    if( is.null( col ) == F ) rect( x1, y1, x2, y2, col=col, border=F, ...)
    if( 1 %in% sides ) segments( x1, y1, x2, y1, col=border, ...)
    if( 2 %in% sides ) segments( x1, y1, x1, y2, col=border, ...)
    if( 3 %in% sides ) segments( x1, y2, x2, y2, col=border, ...)
    if( 4 %in% sides ) segments( x2, y1, x2, y2, col=border, ...)

}

