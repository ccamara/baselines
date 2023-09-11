

canvas_grid <- function( x_left=0, x_right=1,
                         y_bottom=0, y_top=1,
                         col="#dbe4f0", lwd=0.5, ... ){

  previous_par <- par()

  par( fig=c( x_left, x_right, y_bottom, y_top ), new=T )
  par( mar=c( 0, 0, 0, 0 ) )

  plot_blank( xaxs="i", yaxs="i" )

  x_Sequence <- seq( x_left, x_right, by = 0.01)
  y_Sequence <- seq( y_bottom, y_top, by = 0.01)

  segments( x_Sequence, 0, x_Sequence, 1, col=adjustcolor( col=col, alpha.f = 0.25 ), lwd=0.5*lwd )
  segments( 0, y_Sequence, 1, y_Sequence, col=adjustcolor( col=col, alpha.f = 0.25 ), lwd=0.5*lwd )


  x_Sequence <- seq( x_left, x_right, by = 0.1)
  y_Sequence <- seq( y_bottom, y_top, by = 0.1)

  segments( x_Sequence, 0, x_Sequence, 1, col=col, lwd=lwd )
  segments( 0, y_Sequence, 1, y_Sequence, col=col, lwd=lwd )

  text( x_Sequence, 0, labels= x_Sequence, pos=3, col=col, cex=1)
  text( 1, y_Sequence, labels= y_Sequence, pos=2, col=col, cex=1)

  restore_par( previous_par )

}
