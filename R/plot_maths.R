plot_maths <- function( x=-1:1, y=-0.5:0.5 ){
  
  xy_max <- ceiling( max(x, y) )
  
  plot_blank( -xy_max:xy_max, -xy_max:xy_max, asp=1 )
  
  axis_ticks( 1, at =-xy_max:xy_max, length = 1, col="lightblue" )
  axis_ticks( 2, at =-xy_max:xy_max, length = 1, col="lightblue")
  
  axis_outline_arrows( col="grey50")  
  
  axis_labels( 1, at =-xy_max:xy_max, las=2)
  axis_labels( 2, at =-xy_max:xy_max, las=2)
  
}