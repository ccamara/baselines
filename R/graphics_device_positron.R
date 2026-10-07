#' Set Postriron's device as default graphics device in options(...)
#'
#' @return
#' @export
#'
#' @examples
graphics_device_rstudio <- function(){
  options(device=".ark.graphics.device")
}
