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
