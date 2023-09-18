#' Restore the figure region parameters (but not margins)
#'
#' @param previous_par
#'
#' @return
#' @export
#'
#' @examples

restore_figure_region <- function( previous_par ){

  par(fig = previous_par$fig )

}
