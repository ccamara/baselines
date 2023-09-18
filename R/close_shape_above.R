#' Create a shape for a polygon upwards from your data
#'
#' @param shape values for the coordinates of the data/shape to be closed upwards
#' @param left_to_right a logical parameter specifying whether the data is read left to right or not
#' @param extreme_value a value defining the top of the new shape
#'
#' @return a data frame of coordinates for the new, closed shape
#' @export
#'
#' @examples

close_shape_above <- function( shape, left_to_right=T, extreme_value=NULL ){


  if( is.null( extreme_value) == T ) extreme_value <- max( shape[ , 2] )

  if( shape [1,1] == shape[ dim(shape)[1], 1] &&
      shape [1,2] == shape[ dim(shape)[1], 2] ){
    return( shape  )
    warning("shape already closed - no action taken")

  }else{

    if( left_to_right == T ){

      closed_shape <- as.data.frame( rbind ( shape,
                                             c( max( shape[,1]), extreme_value),
                                             c( min( shape[,1]), extreme_value),
                                             shape[ 1, ]  ) )

    }else{

      closed_shape <- as.data.frame( rbind ( shape,
                                             c( min( shape[,1]), extreme_value),
                                             c( max( shape[,1]), extreme_value),
                                             shape[ 1, ]  ) )

    }

    return( closed_shape )

  }

}
