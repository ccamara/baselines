
#' Add an x axis label (side = 1)
#'
#' @param text the text to be displayed
#' @param col the colour of that text
#' @param cex the size of the text (character explansion)
#' @param font the text formatting - 1, plain; 2, bold; 3, italics; 4, bold italics.
#' @param ... global par() parameters
#'
#' @return
#' @export
#'
#' @examples
#' text_x_label()

text_x_label <- function( text = "x label", col="grey30", cex=1, font=1, ... ){

   title( xlab = text, col.lab=col, cex.lab=cex, font.lab=font, ... )

}
