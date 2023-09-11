
rect_fade <-  function( x1 = par()$usr[1], y1 = par()$usr[3],
                       x2 = par()$usr[2], y2 = par()$usr[4],
                       start_side=1, resolution=20,
                       colours=c( "grey20", "hotpink", "grey80"),
                       border_colour=F, xpd=F, ... ){

  thoseCols <- colorRamp( colors = colours )( seq( 0, 1, l=resolution ) )
  thesecols <- rgb( thoseCols[,1], thoseCols[,2], thoseCols[,3],  maxColorValue = 255)

  x1_series <- rep( x1,  resolution )
  x2_series <- rep( x2,  resolution )
  y1_series <- rep( y1,  resolution )
  y2_series <- rep( y2,  resolution )

  if( start_side == 1 ){
    ySeriesNull <- seq( y1, y2, l=resolution+1 )
    y1_series <- ySeriesNull[1: resolution]
    y2_series <- ySeriesNull[2:(resolution+1)]
  } else if ( start_side ==3 ){
    ySeriesNull <- seq( y2, y1, l=resolution+1 )
    y1_series <- ySeriesNull[1: resolution]
    y2_series <- ySeriesNull[2:(resolution+1)]
  } else if ( start_side ==2 ){
    xSeriesNull <- seq( x1, x2, l=resolution+1 )
    x1_series <- xSeriesNull[1: resolution]
    x2_series <- xSeriesNull[2:(resolution+1)]
  } else if ( start_side == 4){
    xSeriesNull <- seq( x2, x1, l=resolution+1 )
    x1_series <- xSeriesNull[1: resolution]
    x2_series <- xSeriesNull[2:(resolution+1)]
  }

  if( border_colour == T | border_colour == TRUE ){
     rect( x1_series, y1_series, x2_series, y2_series, col=thesecols, border=thesecols, xpd=xpd, ... )
  } else if ( border_colour == F | border_colour == FALSE | is.na(border_colour) | is.null(border_colour)  ) {
    rect( x1_series, y1_series, x2_series, y2_series, col=thesecols, border=NA, xpd=xpd, ... )
  } else {
    rect( x1_series, y1_series, x2_series, y2_series, col=thesecols, border=border_colour, xpd=xpd, ... )
  }

}
