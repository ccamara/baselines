
#' Draw lines piecewise, to add widths or colours for each segment
#'
#' @param x values for the x coordinates
#' @param y values for the y coordinates
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples

lines_as_segments <- function( x, y,
                               #z,
                               ... ){

  segments( x[ 1:(length(x)-1) ], y[ 1:(length(y)-1) ],
            x[ 2:length(y) ], y[ 2:length(y) ], ...  )

}


