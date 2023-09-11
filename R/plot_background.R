
plot_background <- function( col=adjustcolor( "skyblue2", alpha.f = 0.5 ), ... ){

  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col=col, border=col, ... )
  
  
}