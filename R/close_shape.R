#' Add start coordinates as end coordinates to close a shape
#'
#' @param my_shape values for the coordinates of the shape (x,y)
#'
#' @return
#' @export
#'
#' @examples
#' par( mfrow=c(1,2))
#' plot( arc_points(arc_length = 270 ), type='l', asp=1)
#' plot( close_shape( arc_points(arc_length = 270 ) ), type='l', asp=1 )


close_shape <- function( my_shape ){

  if( my_shape [1,1] == my_shape[ dim(my_shape)[1], 1] &&
      my_shape [1,2] == my_shape[ dim(my_shape)[1], 2] ){
      return( my_shape  )
      warning("shape already closed - no action taken")
  }else{
      closed_shape <- as.data.frame( rbind ( my_shape, my_shape[ 1, ]  ) )
      return( closed_shape )
  }

}
