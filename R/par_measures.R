par_measures <- function( ... ){

###___ save dimensions for region/area and each unit

# calculate conversion ratios from knowns
    pxPerInch <- dev.size( "px")[1] / dev.size( "in")[2]
    linesPerInch <- max( par("mar") / par("mai"), na.rm = T )

# plot region dimensions
    plotInches  <- par( "pin" )
    plotCm <- cm( plotInches )
    plotPixels <- pxPerInch * plotInches
    plotLines <- linesPerInch * plotInches
    plotXRange <- par()$usr[1:2]
    plotYRange <- par()$usr[3:4]
    plotXCoordinatesInFigure  <- par()$plt[1:2]
    plotYCoordinatesInFigure  <- par()$plt[3:4]

# margin dimensions
    marginInches <- par("mai")
    marginCm <- cm( marginInches )
    marginPixels <- pxPerInch * marginInches
    marginLines <- linesPerInch * marginInches
    marginXCoordinatesInOuter <-  par( )$omd[1:2]
    marginYCoordinatesInOuter <-  par( )$omd

    # margin extremes as plot coordinates
    tempCalc <- marginInches[1:2] * ( plotXCoordinatesInFigure[2] - plotXCoordinatesInFigure[1] ) / plotInches
    marginXCoordinatesInPlot <- c( plotXRange[1] - tempCalc[1], plotXRange[2] - tempCalc[2] )
    tempCalc <- marginInches[1:2] * ( plotYCoordinatesInFigure[2] - plotYCoordinatesInFigure[1] ) / plotInches
    marginYCoordinatesInPlot <- c( plotYRange[1] - tempCalc[1], plotYRange[2] - tempCalc[2] )

# device dimensions
    outerInches <- dev.size( "in")
    outerCm <- dev.size( "cm")
    outerPixels <- dev.size( "px")
    outerLines <- linesPerInch * outerInches


    ###___ create object (S3 class)

                          # calculate conversion ratios from known
    parMeasures <- list(  pxPerInch = pxPerInch,
                          linesPerInch = linesPerInch,
                          # plot region dimensions
                          plotInches = plotInches,
                          plotCm = plotCm,
                          plotPixels = plotPixels,
                          plotLines = plotLines,
                          plotXRange = plotXRange,
                          plotYRange = plotYRange,
                          plotXCoordinatesInFigure = plotXCoordinatesInFigure,
                          plotYCoordinatesInFigure = plotYCoordinatesInFigure,
                          # margin dimensions
                          marginInches = marginInches,
                          marginCm = marginCm,
                          marginPixels = marginPixels,
                          marginLines = marginLines,
                          marginXCoordinatesInOuter = marginXCoordinatesInOuter,
                          marginYCoordinatesInOuter = marginYCoordinatesInOuter,
                          # margin extremes as plot coordinates
                          marginXCoordinatesInPlot = marginXCoordinatesInPlot,
                          marginYCoordinatesInPlot = marginYCoordinatesInPlot,
                          # device dimensions
                          outerInches = outerInches,
                          outerCm = outerCm,
                          outerPixels = outerPixels,
                          outerLines = outerLines,
                          par = par() )

    class( parMeasures ) <- "ways ting"

    return( parMeasures )

}



