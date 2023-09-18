#' Draw line for axis (without labels or axis ticks)
#'
#' @param side integer specifying which side of the plot the axis is drawn on: 1=below, 2=left, 3=above and 4=right
#' @param col colour of the lines
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

axis_line <- function ( side=NULL, col="grey20", ... ){

  axis( side=side, labels = F, lwd.ticks = 0, tck=0, col=col, ... )

}
