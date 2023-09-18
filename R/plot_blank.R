
#' Create a blank plot
#'
#' @param x values for x, as a sequence or vector, max & min values are used for data range
#' @param y values for y, as a sequence or vector, max & min values are used for data range
#' @param margins logical value specifying if margins are preserved (default), or not
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

plot_blank <-  function( x = 0:1, y = 0:1, margins=T, ... ){

  previous_par <- par()

  if( margins==F ) par( mar=c( 0, 0, 0, 0 ) )

  plot( c( min(x), max(x) ), c( min(y), max(y) ),
        axes=F, ann=F, type='n',  ... )

  return( previous_par$mar )

}


