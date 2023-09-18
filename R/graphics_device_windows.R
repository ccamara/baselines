#' Set windows device as default graphics device in options(...)
#'
#' @return
#' @export
#'
#' @examples
graphics_device_windows <- function(){
  options(device="windows")
}

#' Set RStudioGD device as default graphics device in options(...)
#'
#' @return
#' @export
#'
#' @examples
graphics_device_rstudio <- function(){
  options(device="RStudioGD")
}

#' Set quartz device (mac) as default graphics device in options(...)
#'
#' @return
#' @export
#'
#' @examples
graphics_device_quartz <- function(){
  options(device="quartz")
}

#' Set x11 device as default graphics device in options(...)
#'
#' @return
#' @export
#'
#' @examples
graphics_device_x11 <- function(){
  options(device="x11")
}
