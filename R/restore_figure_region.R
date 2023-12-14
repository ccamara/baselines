#' Restore the figure region parameters (but not margins)
#'
#' @param previous_par the saved par object
#'
#' @return
#' @export
#'
#' @examples
#' previous_par_settings <- par()
#' restore_figure_region( previous_par_settings )

restore_figure_region <- function( previous_par ){

  par(fig = previous_par$fig )

}
