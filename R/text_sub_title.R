
#' Add a sub title to your plot
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
#' text_sub_title()

text_sub_title <- function( text = "sub title", col="grey30", cex=1, font=1, ... ){

  title( sub = text, col.sub=col, cex.sub=cex, font.sub=font, ... )

}
