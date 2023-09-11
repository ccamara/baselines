plot_region_rect <- function(){

  x <- c( par()$usr[1], par()$usr[1], par()$usr[2], par()$usr[2], par()$usr[1] )
  y <- c( par()$usr[3], par()$usr[4], par()$usr[4], par()$usr[3], par()$usr[3] )

  PlotRegionRect <- as.data.frame( cbind( x, y ) )

  return( PlotRegionRect )

}
