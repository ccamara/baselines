

#' return to the blank/plain canvas settings
#'
#' @return
#' @export
#'
#' @examples
#'
#'

pop_to_canvas <- function( ){

    prevPar <- par()

    par( mar = c( 0, 0, 0, 0 ) )
    par( oma = c( 0, 0, 0, 0 ) )

    par( fig = c( 0, 1, 0, 1 ), new=T  )

    plot( 0:1, 0:1, axes=F, ann=F, type='n', xaxs="i", yaxs="i" )

}

