# R cutter function (general use)
# replacement / alternative / evaluation version for autoshading (as in GIStools)

# also add methods from https://gistbok.ucgis.org/bok-topics/2019-quarter-02/statistical-mapping-enumeration-normalization-classification
# e.g. maximum breaks, k means, + head tail breaks

colour_cutter <- function( data = 1:7, colours = grey.colors(7), cutter ="quantile", n_nested_bins = 2 ){

        Ncolours <- length(colours)

        ###___ create bins for cutter selection

                # be kind on capitalisation (not necessarily a good thing!)

                if( cutter == "quantile" || cutter=="Quantile" ){

                  # quantile cuts - bins contain equal sized portions of data
                  quBins <- quantile( x = data, probs=seq( 0, 1, l=Ncolours+1), type=1 )
                  # unname the quantile output to remove %'s
                  theseBins <-  unname(quBins)

                } else if( cutter == "range" || cutter=="Range" ){

                  # range cuts - equal sized bins across data range
                  raBins <- seq( min(data), max(data), l=(Ncolours+1) )
                  theseBins <- raBins

                } else if( cutter=="sd" || cutter=="SD" || cutter=="Sd" ){

                  # sd cuts - standard deviations  - a bit weird
                  # N sd's centred on mean, so it can extend beyond the data

                  # sequence for N sd's with sd bin size
                  sdBins1 <- seq( 0, Ncolours )*sd(data)
                  # add mean
                  sdBins2 <- sdBins1 + mean( data )
                  # centre on half of N colours sd range (same for odd and even)
                  sdBins3 <- sdBins2 - ( sd(data)  * (Ncolours / 2 ) )
                  sdBins <-  sdBins3

                  # if max or min data extends beyond colour range then extend bin(s)
                  if( min( sdBins ) > min(data) ) sdBins[1] <- min( data )
                  if( max( sdBins ) < max(data) ) sdBins[ length( sdBins )] <- max( data )

                  theseBins <- sdBins

                }else if( cutter=="kMeans" || cutter=="kmeans" || cutter=="Kmeans" ){

                  myK <- kmeans( data, centers = Ncolours )

                  # create data frame for min and max values
                  jBins <- as.data.frame( cbind( 1:Ncolours, 1:Ncolours ))
                  names( jBins ) <- c( "min", "max" )
                  # create vector for midpoints
                  midPoints <- 1:( Ncolours-1 )
                  # counter for reordering the centres
                  k=1

                  #loop through centres in ascending order
                  for( j in order( myK$centers, decreasing = F ) ){

                        # get indices for values
                        js <- which( myK$cluster==j )
                        # store min & max for each colour cluster
                        jBins$min[k] <- min( data[ js ] )
                        jBins$max[k] <- max( data[ js ] )
                        # increment counter
                        k <- k+1

                  }

                  # get mid points between max of lower centre and min of higher centre
                  midPoints <- jBins$max[ 1:( Ncolours-1 ) ] + 0.5*( jBins$max[ 2:Ncolours ] - jBins$max[ 1:( Ncolours-1 ) ] )
                  # concatenate the min value, mid points and max for final bins
                  kBins <- c( jBins$min[1], midPoints, jBins$max[ Ncolours ]  )

                  theseBins <- kBins

                } else {

                  # default + ERROR catch
                  # quantile cuts - equal sized portions of data in bins
                  quBins<- quantile( x = data, probs=seq( 0, 1, l=Ncolours+1), type=1 )
                  names(quBins)
                  theseBins <-  unname(quBins)

                }


        ###___ match data to bins

                polyBins <- findInterval( data, theseBins )

                # create vector of colours for each polygon for ease of use
                polyCols <- colours[ polyBins ]

                # get density of colours within bins
                histBins <- hist( x = data, breaks = theseBins, plot = F)


        ###___ create finer bins

                # create bins within bins to guide bin selection
                finerBins <- NULL
                finerBinsCols <- NULL

                for( i in 1:( length( theseBins )-1) ){
                    # append uniform sequence between bin values
                    finerBins <- append( finerBins, seq( theseBins[i], theseBins[i+1], l=n_nested_bins+1)[1:(n_nested_bins)] )
                    # add colour for bin ( i.e. bin in which these bins nest )
                    finerBinsCols <- append( finerBinsCols, rep( colours[i], n_nested_bins ) )
                }

                # apend last value to bins
                finerBins <- append( finerBins, tail( theseBins, n=1) )
                finerBinsCols <- append( finerBinsCols, tail( finerBinsCols, n=1 ) )

                # calculate density in bins within bins
                finerBinsDensity <- hist( x = data, breaks = finerBins, plot = F)


        ###___ create shading object (S3 class) a la autoshading (i.e. bins, cols, class name)

                                     # list of bins
                colBinsInfo <- list( breaks = theseBins,
                                     # list of colours
                                     cols = colours,
                                     # cutter function name
                                     cutterFunction = cutter,
                                     # number of colours
                                     nCols = Ncolours,
                                     # density within colour bins
                                     colDensity = histBins$density,
                                     # the nested bins
                                     binsWithinBins = finerBins,
                                     # density within nested bins
                                     binsWithinBinsDensity = finerBinsDensity$density,
                                     # colour of nest within which nested bins sit
                                     binsWithinBinsCols = finerBinsCols,
                                     # lists by polygon/unit for ease of use
                                     binList = polyBins,
                                     colourList = polyCols  )

                class( colBinsInfo ) <- "shading"


        return( colBinsInfo )

}
