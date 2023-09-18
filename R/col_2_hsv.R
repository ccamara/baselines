#' Get hsv values for colour/hex name
#'
#' @param col
#'
#' @return
#' @export
#'
#' @examples
#' col_2_hsv("slateblue2")
#' col_2_hsv("slateblue2")[1]

col_2_hsv <- function( col ) {

  if( length(col) >1  ){
    colHsv <- rgb2hsv( col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255 )
  }else{
    colHsv <- rgb2hsv( col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], maxColorValue=255 )
  }

  return(colHsv)

}
