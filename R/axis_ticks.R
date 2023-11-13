#' Draw axis tick marks (without labels or axis line)
#' #'
#' @param side integer specifying which side of the plot the axis is drawn on: 1=below, 2=left, 3=above and 4=right
#' @param lwd number for the line width
#' @param col colour of the tick marks
#' @param length distance of tick marks into the plot region (+ve values) or out of the plot region (-ve values)
#' @param tcl insted of relative length, use tcl for fixed length as fraction of height of a line of text
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

axis_ticks <- function ( side=NULL, lwd = 1, col="grey20", length=-0.05, ... ){

  axis( side=side, labels = F, lwd = 0, lwd.ticks = lwd, col.ticks=col, tck=length, ... )

}
