#' Convert cm to inches
#'
#' @param cms values in centimetres to be converted to inches
#'
#' @return
#' @export
#'
#' @examples

cm_2_inches <- function( cms ){

  asInches <- cms/2.54
  return( asInches )

}
