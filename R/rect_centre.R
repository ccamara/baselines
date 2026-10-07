
rect_centre <- function( centre = c(0,0), width=2, height=1, rotation=0, output=F, ... ){

  if( length( centre) != 2 ) message("centre should be 2 values, x and y")

  x1 <- centre[1] - 0.5*width
  x2 <- centre[1] + 0.5*width

  y1 <- centre[2] - 0.5*height
  y2 <- centre[2] + 0.5*height

  this_rect <- NULL
  this_rect$x <- c( x1, x1, x2, x2 )
  this_rect$y <- c( y1, y2, y2, y1 )
  this_rect <- as.data.frame( this_rect )

  if( rotation !=0){
    xy <- rotate( x=this_rect$x, y=this_rect$y, rotation=rotation, centre = NULL )[,1:2]
  } else {
    xy <- this_rect[,1:2]
  }

  names( xy ) <- c( "x", "y" )

  if( output == F ){
    polygon( xy$x, xy$y, ... )
  } else {
    return( xy )
  }

}
