#' layout for a grid of plots in a specified figure region
#'
#' @param grid number or rows and columns in the grid as c( r, c )
#' @param region region of figure to draw into as c(x1, x2, y1, y2)
#' @param type determines whether figures are drawn by column (mfcol) or row (mfrow), defaults to 'mfcol'
#' @param new controls whether the plots are drawn over (T, default) or if the device is reset (F)
#'
#' @return
#' @export
#'
#' @examples
#' canvas_blank( )
#' rect( 0.1, 0.1, 0.4, 0.4, col="pink2", border=F)
#' par_grid_in_region( grid = c( 2, 3 ), region=c( 0.3, 0.7, 0.1, 0.97)  )
#' par( mar=rep(0.5,4) )
#' for( i in 1:6 ){
#'     plot_blank( )
#'     plot_background()
#' }
#' par_grid_in_region( grid=c(3,5), region = c( 0.15, 0.8, 0.5, 0.9) )
#' par( mar=rep(0.25,4) )
#' for( i in 1:15 ){
#'     plot_blank( )
#'     plot_background( "snow3" )
#' }

par_grid_in_region <- function( grid=c(3,4),
                                region=c( 0.15, 0.55, 0.25, 0.85 ),
                                type="mfcol",
                                new=T ){

  if( length( grid ) !=2  ) stop( "grid must contain 2 values as c( rows, columns )" )
  if( length( region ) !=4  ) stop( "region must contain 4 values as c( x1, x2, y1, y2 )" )
  if( region[1] > region[2]   ) stop( "x1 region is greater than x2" )
  if( region[3] > region[4]   ) stop( "y1 region is greater than y2" )

  par( omd = c( region[1], region[2], region[3], region[4] ) )

  if( type=="mfcol" ){
      par( mfcol=c( grid[1], grid[2] ) )
  } else if( type == "mfrow" )  {
      par(mfrow=c( grid[1], grid[2] ) )
  } else {
    warning( "type must be 'mfcol' (plotted by column) or 'mfrow' (plotted by row)" )
  }

  if( new==T ){
    par( mfg =c( 1, 1 ) )
  }


}
