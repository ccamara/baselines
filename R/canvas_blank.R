
# a blank plot given the data i.e. not plot.new() as it has the context of the data


canvas_blank <-  function( asp=NULL, restore_previous_par=F, ... ){

  par_to_restore <- par()

  par( fig=c( 0, 1, 0, 1) )
  par( mar=c( 0, 0, 0, 0 ) )
  par( oma=c( 0, 0, 0, 0) )

  plot_blank( xaxs="i", yaxs="i",  asp=asp, ... )

  # return( par_to_restore$mar  )

  #par()$mar <- previous_par$mar

  #restore_figure_region( )

  if( restore_previous_par == T ){
      restore_par(  par_to_restore  )
  } else {
      restore_par( )
  }

}


