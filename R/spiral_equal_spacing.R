#' Create spiral with equidistant points (approximation of Archimedes spiral)
#'
#' @param turns_to turns in spiral
#' @param turns_from turns to clip from centre of spiral
#' @param n_points integer, number of (approximately) equidistant points on spiral
#' @param rotate rotation for spiral (degrees by default)
#' @param units units for rotation "degrees" (default) or "radians"
#' @param flip_x multiplier for x values to flip direction (-1 reflect in y axis)
#' @param flip_y multiplier for x values to flip direction (-1 reflect in x axis)
#'
#' @return
#' @export
#'
#' @examples
#'par(mfrow=c( 5, 5 ))
#'par(mar=c(0,0,0,0))
#'for( j in c( 0, 2 ) ){
#'  for( i in c( 1, 2, 3, 5, 10 ) ){
#'    my_spiral <- spiral_equal_spacing( turns_to = i, turns_from=j )
#'    plot_blank( -1:1, -1:1, asp=1 )
#'    cnt <- 0
#'    for( k in i:0 ){
#'      cnt <- cnt+1
#'      if( cnt%%2 ==0 ) colr <- "grey98" else colr<- "grey90"
#'      polygon( k/i*arc_points(), col=colr, border=F )
#'    }
#'    lines( my_spiral[[1]]$x, my_spiral[[1]]$y, col="pink2", lwd=2 )
#'    lines(arc_points(), col="grey80", lwd=0.5)
#'    points( my_spiral[[2]]$x, my_spiral[[2]]$y, pch=21, col="white", bg="black",cex=2, lwd=2 )
#'    text( -1, 1, paste("from=", j), adj=0)
#'    text( 1, 1, paste("to=", i), adj=1)
#'  }
#'}
#'for( i in c( 1, 2, 10, 20, 47 ) ){
#'  my_spiral <- spiral_equal_spacing( n_points = i )
#'  plot_blank( -1:1, -1:1, asp=1 )
#'  lines( my_spiral[[1]]$x, my_spiral[[1]]$y, col="orchid", lwd=3 )
#'  lines(arc_points(), col="grey80", lwd=0.5)
#'  points( my_spiral[[2]]$x, my_spiral[[2]]$y, pch=21, col="white", bg="black",cex=2, lwd=2 )
#'  text( 0, 0, "n=", pos=3, cex=3, col="grey70" )
#'  text( 0, 0, i, pos=1, cex=3, col="grey70" )
#'}
#'for( i in c( 0, 90, -90, 167, -193  )){
#'  my_spiral <- spiral_equal_spacing( rotate = i )
#'  plot_blank( -1:1, -1:1, asp=1 )
#'  lines( my_spiral[[1]]$x, my_spiral[[1]]$y, col="cyan3", lwd=3 )
#'  lines(arc_points(), col="grey80", lwd=0.5)
#'  points( my_spiral[[2]]$x, my_spiral[[2]]$y, pch=21, col="white", bg="black",cex=2, lwd=2 )
#'  text( 0, 0, "rotate=", pos=3, cex=2, col="grey70" )
#'  text( 0, 0, i, pos=1, cex=3, col="grey70" )
#'}
#'for( i in c(1,-1) ){
#'  for( j in c(1,-1) ){
#'    my_spiral <- spiral_equal_spacing( flip_x = i, flip_y=j )
#'    plot_blank( -1:1, -1:1, asp=1 )
#'    lines( my_spiral[[1]]$x, my_spiral[[1]]$y, col="lightgreen", lwd=3 )
#'    lines(arc_points(), col="grey80", lwd=0.5)
#'    points( my_spiral[[2]]$x, my_spiral[[2]]$y, pch=21, col="white", bg="black",cex=2, lwd=2 )
#'    text( 0, 0, paste( "flip_x=", i ), pos=3, cex=1.5, col="grey40" )
#'    text( 0, 0, paste( "flip_y=", j ), pos=1, cex=1.5, col="grey40" )
#'  }
#'}
#'i <- (-193/360)*2*pi
#'my_spiral <- spiral_equal_spacing( rotate = i, units="radians" )
#'plot_blank( -1:1, -1:1, asp=1 )
#'lines( my_spiral[[1]]$x, my_spiral[[1]]$y, col="cyan3", lwd=3 )
#'lines(arc_points(), col="grey80", lwd=0.5)
#'points( my_spiral[[2]]$x, my_spiral[[2]]$y, pch=21, col="white", bg="black",cex=2, lwd=2 )
#'text( 0, 0, "RADIANS=", adj=c(0.5, -0.6), cex=1.8, col="grey70" )
#'text( 0, 0, "rotate=", adj=c(0.5, -0), cex=1, col="grey70" )
#'text( 0, 0, round( i, digits=2), pos=1, cex=2, col="grey70" )


spiral_equal_spacing <- function( turns_to = 5, turns_from = 3, n_points=20, rotate = 0, units="degrees", flip_x = 1, flip_y =1 ){

  # add clockwise inward param?
  # add start point instead of rotate?

  # based on:
  # https://stackoverflow.com/questions/33904843/how-can-i-get-a-point-on-a-spiral-given-degrees-of-rotation/33916048

    if( turns_to <= turns_from ){
          turns_from <- 0
          message( " turns_from set to 0, turns_from should be < turns_to" )
    }

    # multiply for radians
    thetamax <- turns_to*2*pi
    hole <- turns_from*2*pi

    # create spiral line as guide
    b <- 0.5/pi
    theta <- seq( hole, thetamax, by=0.1 )
    x <- b*theta*cos(theta)
    y <-  b*theta*sin(theta)

    #rescale to 0-1
    max_val <- max( x, y)
    x <- flip_x*x / max_val
    y <- flip_y*y / max_val

    xy <- rotate( x, y, rotation = rotate, units=units )

    # Calculation of equidistant (xi,yi) points on spiral.
    smax1 <- 0.5*b*((hole/thetamax)*thetamax)^2
    smax2 <- 0.5*b*thetamax^2
    s <-  seq( smax1, smax2, l=n_points )
    thetai <- (2*s/b)^0.5
    # rescaling and flips in calculation
    xi <-  flip_x*(1/ max_val)*b*thetai*cos(thetai)
    yi <- flip_y*(1/ max_val)*b*thetai*sin(thetai)

    xyi <- rotate( xi, yi, rotation=rotate, units=units )

    # remove extra columns form rotation outputs
    spiral_line <- xy[,1:2]
    spiral_points <- xyi[,1:2]

    names( spiral_line ) <- c( "x", "y" )
    names( spiral_points ) <- c( "x", "y" )

    this_spiral <- list( spiral_line[,1:2], spiral_points[,1:2] )

    return( this_spiral )

}

