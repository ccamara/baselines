
#' Add a background colour to the plot region
#'
#' @param col the colour for the background and border
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

plot_background <- function( col=adjustcolor( "skyblue2", alpha.f = 0.5 ), ... ){

  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col=col, border=col, ... )


}
