
rectFade <-  function( x1 = par()$usr[1], y1 = par()$usr[3], x2 = par()$usr[2], y2 = par()$usr[4], 
                       startSide=1, resolution=20, 
                       colours=c( "grey20", "hotpink", "grey80"), border_colour=F, xpd=F, ... ){
  
  #ways.blank.plot()

  #x1 <- 0.2
  #x2 <- 0.7
  #y1 <- 0.04 
  #y2 <- 0.6
  
  thoseCols <- colorRamp( colors = colours )( seq( 0, 1, l=resolution ) )
  thesecols <- rgb( thoseCols[,1], thoseCols[,2], thoseCols[,3],  maxColorValue = 255)
  
  # ADD TRANSPARENCY FADE TO COLOURS AND TRANSPARENCY PARAMETERS
  # ADD Border = present colour option...
  
  # create repeated series
  x1Series <- rep( x1,  resolution )
  x2Series <- rep( x2,  resolution ) 
  y1Series <- rep( y1,  resolution )
  y2Series <- rep( y2,  resolution ) 
  
  if( startSide == 1 ){ 
    ySeriesNull <- seq( y1, y2, l=resolution+1 )
    y1Series <- ySeriesNull[1: resolution]
    y2Series <- ySeriesNull[2:(resolution+1)]
  } else if ( startSide ==3 ){
    ySeriesNull <- seq( y2, y1, l=resolution+1 )
    y1Series <- ySeriesNull[1: resolution]
    y2Series <- ySeriesNull[2:(resolution+1)]
  } else if ( startSide ==2 ){
    xSeriesNull <- seq( x1, x2, l=resolution+1 )
    x1Series <- xSeriesNull[1: resolution]
    x2Series <- xSeriesNull[2:(resolution+1)]
  } else if ( startSide == 4){
    xSeriesNull <- seq( x2, x1, l=resolution+1 )
    x1Series <- xSeriesNull[1: resolution]
    x2Series <- xSeriesNull[2:(resolution+1)]
  }
  
  print( border_colour )

  #rect( x1Series, y1Series, x2Series, y2Series, col=thesecols, border= border, ... )
  
  if( border_colour == T | border_colour == TRUE ){ 
     rect( x1Series, y1Series, x2Series, y2Series, col=thesecols, border=thesecols, xpd=xpd, ... )
  } else if ( border_colour == F | border_colour == FALSE | is.na(border_colour) | is.null(border_colour)  ) {
    rect( x1Series, y1Series, x2Series, y2Series, col=thesecols, border=NA, xpd=xpd, ... )
  } else {
    rect( x1Series, y1Series, x2Series, y2Series, col=thesecols, border=border_colour, xpd=xpd, ... )
  }

}  