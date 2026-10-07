
#' Calculate relative scale of shape sizing given data range and aspect ratio
#'
#'
#' @returns
#' @export
#'
#' @examples

shape_scale_to_figure_region <- function(){

    x_per_inch <- ( par()$usr[2]-par()$usr[1] ) / par()$pin[1]
    y_per_inch <- ( par()$usr[4]-par()$usr[3] ) / par()$pin[2]

    units_ratio <- x_per_inch / y_per_inch
    aspect_ratio <- par()$pin[1] / par()$pin[2]

    scaling_data <- as.data.frame( cbind(  units_ratio, aspect_ratio, x_per_inch, y_per_inch ) )

    return( scaling_data )

}
