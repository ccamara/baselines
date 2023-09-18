#' Create a shape for a polygon to the left of your data
#'
#' @param shape values for the coordinates of the data/shape to be closed to the left
#' @param left_to_right a logical parameter specifying whether the data is read left to right or not
#' @param extreme_value a value defining the top of the new shape
#'
#' @return a data frame of coordinates for the new, closed shape
#' @export
#'
#' @examples

close_shape_left <- function( shape, top_to_bottom=T, extreme_value=NULL ){

  if( is.null( extreme_value) == T ) extreme_value <- min( shape[ , 1] )

  if( shape [1,1] == shape[ dim(shape)[1], 1] &&
      shape [1,2] == shape[ dim(shape)[1], 2] ){

    return( shape  )
    warning("shape already closed - no action taken")

  }else{

    if( top_to_bottom == T ){

        closed_shape <- as.data.frame( rbind ( shape,
                                               c( extreme_value, max( shape[,2]) ),
                                               c( extreme_value, min( shape[,2]) ),
                                               shape[ 1, ]  ) )
    }else{

        closed_shape <- as.data.frame( rbind ( shape,
                                               c( extreme_value, min( shape[,2]) ),
                                               c( extreme_value, max( shape[,2]) ),
                                               shape[ 1, ]  ) )


    }

    return( closed_shape )

  }

}
