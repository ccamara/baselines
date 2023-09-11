

# plot lines piecewise to add widths or colours.

lines_as_segments <- function( x, y, z, ... ){

  if( ( length(y) - 1) < length( z ) ) warning( "too many Z values... check or consider using lines" )
  if( ( length(y) - 1) > length( z ) ) warning( "not enough Z values... check or consider using lines" )

  segments( x[ 1:(length(x)-1) ], y[ 1:(length(y)-1) ],
            x[ 2:length(y) ], y[ 2:length(y) ], ...  )

}


