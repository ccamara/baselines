
overlay_outer_margin <- function(){

      # save previous par settings with ways.par functions
      prevPar <- par_measures()


          ###___ set colours

          bluePrintColours <- c( "#0C374E", "#2A596F", "#558FA9", "#273A40", "#6E736F", "#B7B2A6", "#EDDECD" )
          tangleColours <- c( '#E63EA0', '#8CC44D', '#F1EECD', '#E9F5EB', '#13222B' )

          colDevRegion <- tangleColours[2]
          colDevLines1 <- tangleColours[4]
          colDevLines2 <- tangleColours[5]
          colDevText <- tangleColours[4]
          alphaDev <- 0.7

          colOuterMarginLine1 <- tangleColours[5]
          colOuterMarginLine2 <- tangleColours[4]
          colOuterMarginText <- tangleColours[4]

      ###___ OUTER MARGINS overlay - whole device to figure area

      #     _________
      #    |@@@@@@@@@|
      #    |@|  _  |@|
      #    |@| |_| |@|
      #    |@|_____|@|
      #    |@@@@@@@@@|

      # plot to whole area
      par( fig=c( 0, 1, 0, 1), new=T )
      # remove margins
      par( mar=c( 0, 0, 0, 0 ) )
      # and remove outer margins
      par( oma=c( 0, 0, 0, 0 ) )

      plot( 0:1, 0:1, type='n',
            ann=F, axes=F,
            xaxs="i", yaxs="i")

      # create vectors for square in square
      xps <- c( prevPar$par$omd[1], prevPar$par$omd[1], prevPar$par$omd[2], prevPar$par$omd[2], prevPar$par$omd[1],
                0,0,1,1,0 )
      yps <- c( prevPar$par$omd[3], prevPar$par$omd[4], prevPar$par$omd[4], prevPar$par$omd[3], prevPar$par$omd[3],
                0,1,1,0,0 )
      # draw polygon
      polygon(xps, yps,
              col=adjustcolor( colDevRegion, alpha.f = alphaDev ) , border=F)



      ###___ plot region only for OUTER margins


      #     _________
      #    |  _____  |
      #    | |  _  | |
      #    | | |@| | |
      #    | |_____| |
      #    |_________|


      #reset( prevPar )

      # reset plot to whole device area
      par( fig=c( 0, 1, 0, 1), new=T )

      par( oma= rep(0,4))

      par(mar= prevPar$par$oma)

      # blank plot scaled 0 to 1 ... enables easy annotations
      ### !!! note x axis is reversed for margin positioning !!!
      plot( 0:1, 0:1, type='n', ann=F, axes=F,
            xaxs=prevPar$par$xaxs,
            yaxs=prevPar$par$yaxs,
            xlim=c(1,0) )

      ###___ add lines for OUTER margins

      thisStart <- 0.3
      thisEnd <- 0.4
      thisEnd2 <- 0.5

      #
      for( i in 1:4 ) {
        for( j in 0:round( prevPar$par$oma[i], digits=0 ) ) {

          for( k in seq( j, j+1, by=0.2 ) ){
            axis( i, at=c( thisStart, thisEnd ), line=k, outer=F,
                  tck=0, lwd=2, labels=F, col=colOuterMarginLine1, lty=1, xpd=T  )
            axis( i, at=c( thisStart, thisEnd ), line=k, outer=F,
                  tck=0, lwd=0.5, labels=F, col=colOuterMarginLine2, lty=1, xpd=T  )
          }


          axis( i,
                at=c( thisStart, thisEnd2 ),
                line=j,
                outer=F,
                tck=0, lwd=4, labels=F, col=colOuterMarginLine1, lty=1, xpd=T  )

          axis( i,
                at=c( thisStart, thisEnd2 ),
                line=j,
                outer=F,
                tck=0, lwd=2, labels=F, col=colOuterMarginLine2, lty=1, xpd=T  )


          axis( i, at = thisEnd2+ 0.2*(thisEnd2-thisStart),
                line = j-1.2, labels = j,
                tck = 0, lwd = 0, las=1, cex.axis=0.75,
                col = colOuterMarginText, lty = 1, xpd = T, outer=F )


        }
      }


      # strange behaviour when whole list given to which
      removeElementsWithWarnings <- c( which( names( prevPar$par )==c( "cin")),
                                       which( names( prevPar$par )==c( "cra" ) ),
                                       which( names( prevPar$par )==c( "csi" ) ),
                                       which( names( prevPar$par )==c( "cxy", "din", "page" ) ) )


      # reset par settings to previous par
      par( prevPar$par[ - removeElementsWithWarnings ] )

      par( mfg = c( prevPar$par$mfg )  )
      # par( mfrow = c() )


      # add blank plot to move to next plot in mfg
        plot_blank()


}
