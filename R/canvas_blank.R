
#' create a blank canvas to draw onto, with no margins (or outer margins) and whole device
#'
#' @param asp aspect ratio in the  canvas
#' @param restore_previous_par logical value specifying if previous par parametrs should be restored, defaulting to FALSE
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

canvas_blank <-  function( asp=NULL, restore_previous_par=F, ... ){

  par_to_restore <- par()

  par( fig=c( 0, 1, 0, 1) )
  par( mar=c( 0, 0, 0, 0 ) )
  par( oma=c( 0, 0, 0, 0) )

  plot_blank( xaxs="i", yaxs="i",  asp=asp, ... )

  # return( par_to_restore$mar  )

  #par()$mar <- previous_par$mar

  #restore_figure_region( )

  if( restore_previous_par == T ){
      restore_par(  par_to_restore  )
  } else {
      restore_par( )
  }

}


