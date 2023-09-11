
# get hex code for colour/hex name

col_2_hex <- function( col ){

  if( length(col) >1  ){
      colHex<- rgb( col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255)
  }else{
    colHex<- rgb( col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], maxColorValue=255)
  }

      return(colHex)

}


# get hsv code for colour/hex name

col_2_hsv <- function( col ) {

  if( length(col) >1  ){
      colHsv <- rgb2hsv( col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255 )
  }else{
      colHsv <- rgb2hsv( col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], maxColorValue=255 )
  }

  return(colHsv)

}

