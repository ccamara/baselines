#' Draw a grid overlaying the canvas to determine object poistions and coordinates
#'
#' @param x1 left position of the grid (defaulting to whole device)
#' @param x2 right grid position (defaulting to whole device)
#' @param y1 bottom position of grid (defaulting to whole device)
#' @param y2 top position of grid (defaulting to whole device)
#' @param col colour for the grid with 0.5 transparency applied
#' @param lwd line width for grid
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples
#' plot(1:9)
#' canvas_grid()

canvas_grid <- function( x1=0, x2=1,
                         y1=0, y2=1,
                         col="#dbe4f0", lwd=0.5, ... ){

  previous_par <- par()

  par( fig=c( x1, x2, y1, y2 ), new=T )
  par( mar=c( 0, 0, 0, 0 ) )

  plot_blank( xaxs="i", yaxs="i" )

  x_Sequence <- seq( x1, x2, by = 0.01)
  y_Sequence <- seq( y1, y2, by = 0.01)

  segments( x_Sequence, 0, x_Sequence, 1, col=adjustcolor( col=col, alpha.f = 0.25 ), lwd=0.5*lwd )
  segments( 0, y_Sequence, 1, y_Sequence, col=adjustcolor( col=col, alpha.f = 0.25 ), lwd=0.5*lwd )


  x_Sequence <- seq( x1, x2, by = 0.1)
  y_Sequence <- seq( y1, y2, by = 0.1)

  segments( x_Sequence, 0, x_Sequence, 1, col=col, lwd=lwd )
  segments( 0, y_Sequence, 1, y_Sequence, col=col, lwd=lwd )

  text( x_Sequence+0.005, 0.01, labels= x_Sequence, adj=0, col=col, cex=1)
  text( x_Sequence-0.005, 0.99, labels= x_Sequence, adj=1, col=col, cex=1)

  text( 0.005, y_Sequence, labels= y_Sequence, adj=c( 0, 0), col=col, cex=1)
  text( 0.995, y_Sequence, labels= y_Sequence, adj=c( 1, 1),col=col, cex=1)

  restore_par( previous_par )

}
