canvas_rect <- function( x_left = 0.1, y_bottom = 0.5, x_right = 0.9, y_top = 0.9,
                      border=T, col="lemonchiffon", angle=NULL,  ... ){

  # save par settings
  par_to_restore <- par()

  # set plot region to rect area
  par( fig = c( x_left, x_right, y_bottom, y_top ), new=T )
  #remove margins
  par( mar=c(0,0,0,0), oma=c(0,0,0,0) )

  #ways.blank.plot( x = 1:2, ... )
  plot_blank( xaxs="i", yaxs="i" )

  # add rect to fill plot region and so rect area
  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4],
        border=border, col=col, angle=angle, ...  )

  restore_par( par_to_restore )
  restore_figure_region( par_to_restore )

}
