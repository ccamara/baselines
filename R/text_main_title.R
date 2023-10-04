
#' Add a main title to your plot
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
#' text_main_title()

text_main_title <- function( text = "title", col="grey30", cex=1.2, font=2, ... ){

  title( main = text, col.main=col, cex.main=cex, font.main=font, ... )

}
