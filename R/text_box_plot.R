


#' Draw text box in plot region
#'
#' @param x1 value for left, horizontal position in plot region coordinates, defaults to plot region
#' @param y1 value for bottom, vertical position in plot region coordinates, defaults to plot region
#' @param x2 value for right, defaults to plot region
#' @param y2 value for top, defaults to plot region
#' @param text a character vector or expression specifying the text to be written with "Lorem ipsum..." default
#' @param cex text size, character expansion value (see par)
#' @param font default to the values of the global graphical parameters in par()
#' @param family default to the values of the global graphical parameters in par()
#' @param adj alignment of text (but see arguments below)
#' @param srt rotation (not currently implemented)
#' @param border logical or value for the colour of the box border
#' @param lty value for the line type, e.g. 1, 2, 3 or 4... or "22"
#' @param xpd logical value for whether text/shapes extend beyond plot region
#' @param col_box fill colour of the box
#' @param col_text colour of the text
#' @param left_margin padding around text within box
#' @param right_margin padding around text within box
#' @param top_margin padding around text within box
#' @param bottom_margin padding around text within box
#' @param line_spacing padding between lines
#' @param vertical_alignment top, centre or bottom alignment
#' @param horizontal_alignment left, middle or right alignment
#' @param ... other parameters, see global graphical parameters in par()
#'
#' @return
#' @export
#'
#' @examples
#' plot_blank()
#' text_box_plot()

text_box_plot <- function( x1 = par()$usr[1], y1 = par()$usr[3], x2 = par()$usr[2], y2 = par()$usr[4],
                         text = NULL,
                         cex=1, font=1, family="serif",
                         adj=0, srt=0,
                         border=F,
                         lty=1, xpd=T,
                         col_box="grey97",
                         col_text="grey30",
                         left_margin = 0.075,
                         right_margin = 0.075,
                         top_margin = 0.025,
                         bottom_margin = 0.05,
                         line_spacing = 0.05,
                         vertical_alignment = "top",
                         horizontal_alignment = "left", ... ){

  # use 'lorem...' text for default
  if( is.null( text) == T ) text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

  rect( x1, y1, x2, y2,
        border=border, col=col_box, lty=lty, xpd=xpd, ... )

  textCache <- strsplit( text, split = " " )[[1]]

  lineCounter <- 1

  textBoxWidth <- x2 - x1
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

  if( vertical_alignment == "top" || vertical_alignment == "Top" ){

    textTop <- y2 - top_margin*y2

  } else if ( vertical_alignment == "bottom" || vertical_alignment == "Bottom" ){

    textTop <- y1 +
      sum( max( textMarkUp$textLine )*( maxTextHeight + line_spacing*maxTextHeight )  ) +
      bottom_margin*(y1-y2)

  } else if ( vertical_alignment == "middle" || vertical_alignment == "Middle" ){

    textTop <- y1 + 0.5*( y1-y2 ) +
      0.5*sum( max( textMarkUp$textLine )*( maxTextHeight + line_spacing*maxTextHeight )  )

  }


  # set start position with HORIZONTAL alignments

  if( horizontal_alignment == "left" || horizontal_alignment == "Left" ){

    textLeft <- x1 + left_margin*textBoxWidth
    adjNow <- 0

  } else if ( horizontal_alignment == "centre" || horizontal_alignment == "Centre" ){

    textLeft <- x1 + left_margin*textBoxWidth + 0.5*textAreaWidth
    adjNow <- 0.5

  } else if ( horizontal_alignment == "right" || horizontal_alignment == "Right" ){

    textLeft <- x2 - right_margin*textBoxWidth
    adjNow <- 1

  }


  for( thisLine in 1:max( textMarkUp$textLine ) ){

    text( x = textLeft,
          y = textTop - thisLine*( maxTextHeight +line_spacing*maxTextHeight ),
          labels = paste( textCache[ which( textMarkUp$textLine == thisLine ) ], collapse=" " ),
          adj=adjNow, srt=srt, font=font, family=family, cex=cex, xpd=T, col=col_text )

  }

}




