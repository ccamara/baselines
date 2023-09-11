
plot_blank <-  function( x = 0:1, y = 0:1, margins=T, ... ){

  previous_par <- par()

  if( margins==F ) par( mar=c( 0, 0, 0, 0 ) )

  plot( c( min(x), max(x) ), c( min(y), max(y) ),
        axes=F, ann=F, type='n',  ... )

  return( previous_par$mar )

}


