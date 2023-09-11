close_shape_below <- function( shape, left_to_right=T, extreme_value=NULL ){

  if( is.null( extreme_value) == T ) extreme_value <- min( shape[ , 2] )

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
