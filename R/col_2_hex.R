


#' Get hex code for colour/hex name
#'
#' @param col colour to convert to hex code
#'
#' @return
#' @export
#'
#' @examples
#' col_2_hex("hotpink")

col_2_hex <- function( col ){

  if( length(col) >1  ){
      colHex<- rgb( col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255)
  }else{
    colHex<- rgb( col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], maxColorValue=255)
  }

      return(colHex)

}




