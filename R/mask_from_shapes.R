
mask_from_shapes <- function( inner_shape,
                            outer_shape = plot_region_rect(),
                            draw = T,
                            border=F, col="skyblue4", ...){

  # ADD check inner is within outer....

  # do we need to close the shape?
  # i.e. when rotated it is effectively closed and we should close it then to seal the gap?
  #inner_shape <- close_shape( inner_shape)
  #outer_shape <- close_shape( outer_shape)

  # set opposite shape directions straight away



  if( shape_direction( inner_shape) == shape_direction( outer_shape) ) {
    inner_shape <- as.data.frame( cbind(  rev( inner_shape[,1] ), rev( inner_shape[,2] ) ) )
  }

  # names(inner_shape) <- names(outer_shape)

  # calc distances from outer_top_right to all inner points
  distance_to_inner <- ( ( outer_shape[1,1] - inner_shape[,1] )^2 +
                         ( outer_shape[1,2] - inner_shape[,2] )^2 )^0.5

  # find nearest inner point
  nearest_inner_point <- sample( which( distance_to_inner == min(distance_to_inner) ),
                                        size=1 )

  # rotate polygon data to begin with nearest  point
  if( nearest_inner_point != 1 ){
    new_inner <- close_shape( rbind( inner_shape[ nearest_inner_point:dim(inner_shape)[1], ],
                        inner_shape[ 1:( nearest_inner_point-1), ] ) )
  } else {
    new_inner <- close_shape( inner_shape )
  }

  #reversed_new_inner <- as.data.frame( cbind(  rev( new_inner[,1] ), rev( new_inner[,2] ) ) )
  colnames(new_inner) <-  colnames(outer_shape)

  mask_shape <- rbind( close_shape( outer_shape ), new_inner )

  if( draw ==T ){
    #plot( mask_shape, type='n' )
    polygon( mask_shape, col=col, border=F, ... )

    if( border != F ){
      lines( inner_shape, col=border, ...  )
      lines( outer_shape, col=border, ... )

    }

  }else{

    return( mask_shape )

  }

}



