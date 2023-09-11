

axis_line <- function ( side=NULL, col="grey20", ... ){
  
  axis( side=side, labels = F, lwd.ticks = 0, tck=0, col=col, ... ) 
  
}



axis_ticks <- function ( side=NULL, lwd = 1, col="grey20", length=-0.05, ... ){
  
  axis( side=side, labels = F, lwd = 0, lwd.ticks = lwd, col.ticks=col, tck=length, ... ) 
  
}



axis_labels <- function ( side, col="grey20", cex=1, ... ){
  
  axis( side=side, lwd = 0, lwd.ticks = 0, tck=0, col.axis=col, cex.axis=cex, ... ) 
  
}
