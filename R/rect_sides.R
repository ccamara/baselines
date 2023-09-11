
rect_sides <- function( x1, y1, x2, y2, sides=c(1,2,3,4),
                        col=NULL, border="black", ... ){

    if( is.null( col ) == F ) rect( x1, y1, x2, y2, col=col, border=F, ...)
    if( 1 %in% sides ) segments( x1, y1, x2, y1, col=border, ...)
    if( 2 %in% sides ) segments( x1, y1, x1, y2, col=border, ...)
    if( 3 %in% sides ) segments( x1, y2, x2, y2, col=border, ...)
    if( 4 %in% sides ) segments( x2, y1, x2, y2, col=border, ...)

}

