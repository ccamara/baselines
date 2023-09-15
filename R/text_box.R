# NOT DONE PROPERLY  - rotation?

# add in *italic*, **bold** and ***bold and italic*** with simple markdown markup
# i.e. recognise stops, and write separate chunks within line using font=1:4

loremText <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

text_box <- function( x1 = 0.3, y1 = 0.1, x2 = 0.9, y2 = 0.4,
                       text = NULL,
                       cex=1, font=1, family="",
                       adj=0, srt=0, border=F, lty=1,
                       col_box="grey97",
                       col_text="grey30",
                       left_margin = 0.025,
                       right_margin = 0.025,
                       top_margin = 0.025,
                       bottom_margin = 0.025,
                       line_spacing = 0.05,
                       vertical_alignment = "top",
                       horizontal_alignment = "left", ... ){


  # use 'lorem...' text for default
  if( is.null( text) == T ) text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  # save par values
  beforePar <- par()

  par( fig = c( x1, x2, y1, y2 ), new=T )
  par( mar=c(0,0,0,0), oma=c(0,0,0,0) )

  plot_blank(  )

  rect( par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4],
        border=border, col=col_box, lty=lty )

  textCache <- strsplit( text, split = " " )[[1]]

  lineCounter <- 1

  textBoxWidth <- par()$usr[2] - par()$usr[1]
  textAreaWidth <- textBoxWidth - left_margin*textBoxWidth - right_margin*textBoxWidth

  nParts <- length( textCache)

  textMarkUp <- NULL
  textMarkUp$textOrder <- 1:nParts
  textMarkUp$textLine <-  rep(0, nParts )
  textMarkUp$textWidth <-  rep(0, nParts )
  textMarkUp$textSumLineWidth <-  rep(0, nParts )
  textMarkUp$textHeight <-  rep(0, nParts )
  textMarkUp <- as.data.frame( textMarkUp )

  lineStart <- 1
  lineCounter <- 1

  for( partOfText in 1:nParts ){

        textMarkUp$textWidth[ partOfText ] <- strwidth( textCache[ partOfText ],
                                                        font=font, cex=cex, units="user"  ) +
                                              strwidth( " ",
                                                        font=font, cex=cex, units="user"  )

        textMarkUp$textHeight[ partOfText ] <- strheight( textCache[ partOfText ],
                                                          font=font, cex=cex, units="user"  ) +
                                               strheight( " ",
                                                          font=font, cex=cex, units="user"  )

        textMarkUp$textLine[ partOfText ]  <- lineCounter

        if( sum( textMarkUp$textWidth[ lineStart:partOfText ] ) >= textAreaWidth ){
              textMarkUp$textSumLineWidth[ lineStart:( partOfText - 1 ) ] <- sum( textMarkUp$textWidth[ lineStart:( partOfText - 1 ) ] )
              lineCounter  <- lineCounter + 1
              textMarkUp$textLine[ partOfText ] <- lineCounter
              lineStart <- partOfText
        }

        if( partOfText == nParts ){
              textMarkUp$textSumLineWidth[ lineStart:( partOfText ) ] <- sum( textMarkUp$textWidth[ lineStart:( partOfText ) ] )
        }

  }


  maxTextHeight <- max( textMarkUp$textHeight )


  adjNow <- -99

  textTop <- -99
  textLeft <- -99

  #vertical_alignment <- "bottom"
  #horizontal_alignment <- "right"


  # set start position with VERTICAL alignments

  if( vertical_alignment == "top" || vertical_alignment == "Top" ){

      textTop <- par()$usr[4] - top_margin*par()$usr[4] - par()$usr[3]

  } else if ( vertical_alignment == "bottom" || vertical_alignment == "Bottom" ){

      textTop <- par()$usr[3] +
                  sum( max( textMarkUp$textLine )*( maxTextHeight + line_spacing*maxTextHeight )  ) +
                  bottom_margin*par()$usr[4] - par()$usr[3]

  } else if ( vertical_alignment == "middle" || vertical_alignment == "Middle" ){

      textTop <- par()$usr[3] + 0.5*( par()$usr[4] - par()$usr[3] ) +
                  0.5*sum( max( textMarkUp$textLine )*( maxTextHeight + line_spacing*maxTextHeight )  )

  }


  # set start position with HORIZONTAL alignments

  if( horizontal_alignment == "left" || horizontal_alignment == "Left" ){

      textLeft <- par()$usr[1] + left_margin*textBoxWidth
      adjNow <- 0

  } else if ( horizontal_alignment == "centre" || horizontal_alignment == "Centre" ){

      textLeft <- par()$usr[1] + left_margin*textBoxWidth + 0.5*textAreaWidth
      adjNow <- 0.5

  } else if ( horizontal_alignment == "right" || horizontal_alignment == "Right" ){

      textLeft <- par()$usr[2] - right_margin*textBoxWidth
      adjNow <- 1

  }


  for( thisLine in 1:max( textMarkUp$textLine ) ){

    text( x = textLeft,
          y = textTop - thisLine*( maxTextHeight +line_spacing*maxTextHeight ),
          labels = paste( textCache[ which( textMarkUp$textLine == thisLine ) ], collapse=" " ),
          adj=adjNow, srt=srt, font=font, family=family, cex=cex, xpd=T, col=col_text )

    #print( par()$usr[4]-(i+1)*(textHeight-line_spacing* textHeight) )

  }


  restore_par( beforePar )
  restore_figure_region( beforePar )

  #return( beforePar$fig )


}




