
#' Title Quickly add arrows for axes at for x=0 and y=0
#'
#' @param col colour of axis, light grey as default
#' @param length length of arrow head, default 0.1
#' @param angle angel of arrow head, default 45 degrees
#' @param code double arrow head by default (code=2), max only (code=1), min only (code=2)
#' @param xpd logical, draw outside figure region (default TRUE)
#' @param ... parameters taken by arrows function (as per lines, segments etc...)
#'
#' @return
#' @export
#'
#' @examples
#' plot_blank()
#' axis_outline_arrows()

axis_outline_arrows <- function( col="grey75",  length = 0.1, angle=45,  code=3, xpd=T, ... ){

  arrows( c( par()$usr[1], 0),
          c(0, par()$usr[3]),
          c(par()$usr[2],0),
          c(0, par()$usr[4]),
          col=col, length=length, angle=angle, xpd=xpd, code=code, ... )

}
