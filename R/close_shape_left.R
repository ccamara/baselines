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
