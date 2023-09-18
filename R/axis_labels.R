#' Draw axis labels (without axis line or ticks)
#'
#' @param side integer specifying which side of the plot the axis is drawn on: 1=below, 2=left, 3=above and 4=right
#' @param col colours of the labels
#' @param cex size of labels following the cex, character expansion argument of par()
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

axis_labels <- function ( side, col="grey20", cex=1, ... ){

  axis( side=side, lwd = 0, lwd.ticks = 0, tck=0, col.axis=col, cex.axis=cex, ... )

}
