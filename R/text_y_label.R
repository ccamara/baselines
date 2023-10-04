
#' Add an y axis label (side = 2)
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
#' text_y_label()

text_y_label <- function( text = "y label", col="grey30", cex=1, font=1, ... ){

  title( ylab = text, col.lab=col, cex.lab=cex, font.lab=font, ... )

}
