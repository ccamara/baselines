
# return the settings to blank/plain canvas

pop_to_canvas <- function( ){

    prevPar <- par()

    # add visual error?! i.e. if "figure margins too large"

    par( mar = c( 0, 0, 0, 0 ) )
    par( oma = c( 0, 0, 0, 0 ) )

    par( fig = c( 0, 1, 0, 1 ), new=T  )

    plot( 0:1, 0:1, axes=F, ann=F, type='n', xaxs="i", yaxs="i" )

}

