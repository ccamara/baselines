
#' Find direction of rotation for shape coordinates
#'
#' @param shape coordinates for shape
#' @param draw logical value specifying whether shape is drawn or not, defaults to false
#'
#' @return
#' @export
#'
#' @examples


shape_direction <- function( shape, draw=FALSE ){

    # using crisp solution https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order

    # create centre/origin for shape
    centre_x <-  min( shape[, 1] ) + 0.5*( max( shape[, 1] ) - min( shape[, 1] ))
    centre_y <-  min( shape[, 2] ) + 0.5*( max( shape[, 2] ) - min( shape[, 2] ))

    next_point <- rbind( shape[2:(dim(shape)[1]), ], shape[1,] )
    sum_edges <- sum( (next_point[,1]-shape[,1] )*( next_point[,2]+shape[,2] ) )
    #print(sum_edges)
    if( sum_edges >= 0 ){
      shape_clockwise=T
    } else{
      shape_clockwise=F
    }

    if( draw==T ){
      plot( shape, pch=19 )
      # background shape fill
      polygon( shape, border=F, col= "grey90" )
      # start point (square symbol)
      points( shape[1,1], shape[1,2], col="hotpink3", cex=3, pch=15 )
      # arrow directions around shape (apart from last leg)
      arrows( shape[1:( dim(shape)[1] -2 ), 1], shape[1:( dim(shape)[1] -2 ), 2],
              shape[2:( dim(shape)[1] -1 ), 1], shape[2:( dim(shape)[1] -1 ), 2],
              length = 0.125 )
      # last leg as dotted line
      segments( shape[dim(shape)[1] -1 , 1], shape[dim(shape)[1] -1, 2],
                shape[dim(shape)[1], 1], shape[dim(shape)[1], 2], lty=2 )


      if( shape_clockwise == T ){
        this_col <- adjustcolor( "hotpink3", alpha.f = 0.5 )
        text( centre_x, centre_y, labels="Clock",
              col=this_col, cex=3, font=3, pos=1 )
        text( centre_x, centre_y, labels=shape_clockwise,
              col=this_col, cex=3, font=2, pos=3 )
        arrows( par()$usr[1] + 0.8*(par()$usr[2]-par()$usr[1]),
                par()$usr[3] + 0.8*(par()$usr[4]-par()$usr[3]),
                par()$usr[1] + 0.8*(par()$usr[2]-par()$usr[1]),
                par()$usr[3] + 0.2*(par()$usr[4]-par()$usr[3]),
                col=this_col, length=0.2, lwd=3 )
      }else{
        this_col <- adjustcolor( "skyblue2", alpha.f =0.5 )
        text( centre_x, centre_y, labels="Anti",
              col=this_col, cex=3, font=3, pos=1 )
        text( centre_x, centre_y, labels=shape_clockwise,
              col=this_col, cex=3, font=2, pos=3 )
        arrows( par()$usr[1] + 0.8*(par()$usr[2]-par()$usr[1]),
                par()$usr[3] + 0.2*(par()$usr[4]-par()$usr[3]),
                par()$usr[1] + 0.8*(par()$usr[2]-par()$usr[1]),
                par()$usr[3] + 0.8*(par()$usr[4]-par()$usr[3]),
                col=this_col, length=0.2, lwd=3 )
      }


      points( centre_x, centre_y, pch=19, cex=2 )

    }


    return( shape_clockwise )

}

