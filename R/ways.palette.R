bluePrintColours <- c( "#0C374E", "#2A596F", "#558FA9", "#273A40", "#6E736F", "#B7B2A6", "#EDDECD" )


ways.palette <- function( myCols ){
  
  plot( x=seq(0, length(myCols), by=1), 
        y=rep(0, length(myCols)+1), 
        ylim=c(0,1), ann=F, axes=F, type='n', xaxs="i"  )
  
  rect( -1*length(myCols), 0, 2*length(myCols), 0.3, xpd=T, col="grey50", border=F )  
  rect( -1*length(myCols), 0.3, 2*length(myCols), 0.6, xpd=T, col="grey10", border=F )
  
  rect( seq( 0, length(myCols)-1 ), 0,
        seq( 1, length(myCols) ), 0.8, 
        col= myCols, border=F)      
  
  text( seq(0.5, length(myCols)-0.5, by =1), 0.85, labels=1:length(myCols),
        xpd=T, col="grey30", cex=1, srt=90  )
  
  rect( seq( 0.1, length(myCols)-0.9, by=1 ), 0.875,
        seq( 0.9, length(myCols)-0.1, by=1 ), 1, 
        col= myCols, border=F)  
  
  rect( -1*length(myCols), 0.1, 2*length(myCols), 0.2, xpd=T, col="grey50", border=F )  
  rect( -1*length(myCols), 0.4, 2*length(myCols), 0.5, xpd=T, col="grey10", border=F )
  
  text( seq(0.5, length(myCols)-0.5, by =1), -0.1, labels=myCols,
        xpd=T, col="grey50", cex=1, srt=90  )
  
  title( main = deparse(substitute(myCols)), cex.main=3, adj=0, col.main="grey20"   )
  
}

# ways.palette( bluePrintColours )
