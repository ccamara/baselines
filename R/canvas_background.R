
#' Add a background colour to the canvas, whole device
#'
#' @param col the colour for both the background and border
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

canvas_background <- function( col=adjustcolor( "orchid", alpha.f = 0.4 ), ... ){

  previous_par <- par()

  par( fig = c( 0, 1, 0, 1 ), new=T )
  par( oma = rep( 0, 4) )
  par( mar = rep( 0, 4) )

  plot_blank()

  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col=col, border=col, ... )

  restore_par( previous_par =  )

  par( new=T )

}
