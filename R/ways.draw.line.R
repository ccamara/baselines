DrawLine <- function( x1 = 0.1, y1 = 0.1, x2 = 0.7, y2 = 0.3, 
                      col="lemonchiffon3", lwd=2, ... ){
  
  # save par settings
  beforePar <- par()
  
  # set plot region to rect area
  par( fig = c( 0, 1, 0, 1 ), new=T )
  #remove margins
  par( mar=c( 0, 0, 0, 0 ), oma=c( 0, 0, 0, 0) )
  
  #ways.blank.plot( )
  blank_canvas()
  
  # add segments line  
  segments( x1, y1, x2, y2, 
            col=col, lwd=lwd, ... )
  
  # reset par to previous
  RestorePar( beforePar )
  RestoreFigureRegion( beforePar )
  
}