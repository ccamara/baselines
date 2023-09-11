
# a blank plot given the data i.e. not plot.new() as it has the context of the data


canvas_blank <-  function( x = 0:1, y = 0:1, asp=NULL, xaxs="i", yaxs="i", ... ){

  previous_par <- par()

  par( mar=c( 0, 0, 0, 0 ) )

  plot( x, y, axes=F, ann=F, type='n', asp=asp, xaxs=xaxs, yaxs=yaxs,  ... )

  return( previous_par$mar )

}


