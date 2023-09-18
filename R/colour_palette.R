
#' Display a list of colours with hex codes and against backgrounds
#'
#' @param colours list of colours to be displayed
#'
#' @return
#' @export
#'
#' @examples
#' colour_palette()

colour_palette <- function( colours = grey.colors(15) ){

  plot( x=seq(0, length(colours), by=1),
        y=rep(0, length(colours)+1),
        ylim=c(0,1), ann=F, axes=F, type='n', xaxs="i"  )

  rect( -1*length(colours), 0, 2*length(colours), 0.3, xpd=T, col="grey50", border=F )
  rect( -1*length(colours), 0.3, 2*length(colours), 0.6, xpd=T, col="grey10", border=F )

  rect( seq( 0, length(colours)-1 ), 0,
        seq( 1, length(colours) ), 0.8,
        col= colours, border=F)

  text( seq(0.5, length(colours)-0.5, by =1), 0.85, labels=1:length(colours),
        xpd=T, col="grey30", cex=1, srt=90  )

  rect( seq( 0.1, length(colours)-0.9, by=1 ), 0.875,
        seq( 0.9, length(colours)-0.1, by=1 ), 1,
        col= colours, border=F)

  rect( -1*length(colours), 0.1, 2*length(colours), 0.2, xpd=T, col="grey50", border=F )
  rect( -1*length(colours), 0.4, 2*length(colours), 0.5, xpd=T, col="grey10", border=F )

  text( seq(0.5, length(colours)-0.5, by =1), -0.1, labels=colours,
        xpd=T, col="grey50", cex=1, srt=90  )

  title( main = deparse(substitute(colours)), cex.main=3, adj=0, col.main="grey20"   )

}

