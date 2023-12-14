#' Quickly print out par settings for inspection
#'
#' @return
#' @export
#'
#' @examples
#'

par_print_summary <- function(){

  print( "par()$usr:" )
  print( paste( "     x axis range", par()$usr[ 1 ], " < x < ", par()$usr[2] ) )
  print( paste( "     y axis range", par()$usr[ 3 ], " < y < ", par()$usr[4] ) )

  print( "par()$fig:" )
  print( paste( "     x figure region", par()$fig[ 1 ], " < x < ", par()$fig[2] ) )
  print( paste( "     y figure region", par()$fig[ 3 ], " < y < ", par()$fig[4] ) )

  print( "par()$mar:" )
  print( "     margins around plot" )
  print( paste( "     bottom (1)", par()$mar[ 1 ] ) )
  print( paste( "     left   (2)", par()$mar[ 2 ] ) )
  print( paste( "     top    (3)", par()$mar[ 3 ] ) )
  print( paste( "     right  (4)", par()$mar[ 4 ] ) )

  print( "par()$oma:" )
  print( "     outer margins" )
  print( paste( "     bottom (1)", par()$oma[ 1 ] ) )
  print( paste( "     left   (2)", par()$oma[ 2 ] ) )
  print( paste( "     top    (3)", par()$oma[ 3 ] ) )
  print( paste( "     right  (4)", par()$oma[ 4 ] ) )

  print( paste( "default graphics device is set to:", options("device") ) )

}
