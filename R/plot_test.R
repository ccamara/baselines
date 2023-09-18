
#' Create a test plot ( with data and inner and outer margins)
#'
#' @return
#' @export
#'
#' @examples
#' plot_test()
#' box_outer()
#' overlay_outer_margin()
#' axis_labels( side=4, col="hotpink", cex=2 )
#' overlay_margin()


plot_test <-  function(){

  #dev.new()

  par( mar=c(3,10,6,4) )
  par(oma = c(2,5,3,1))

  x <- runif(20, 0, 1)
  y <- runif(20, 0, 1)
  z <- runif(20, 3, 10)

  plot (x, y, cex=1.2*z, pch=19, xaxs="i")
  points (x, y, cex=z, pch=19, col="white")

  for( s in 1:4 ) mtext( side=s, text="OUTER MARGINS OUTER MARGINS",
                         cex=3, outer=T, line=par()$oma[s]/2, col="grey60")

}
