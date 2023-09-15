
restore_par <- function( previous_par = NULL, ... ){

      if( is.null( previous_par ) == T ) {

            default_par <- NULL

            default_par$xlog <- FALSE
            default_par$ylog <- FALSE

            default_par$adj <- 0.5
            default_par$ann <- TRUE
            default_par$ask <- FALSE

            default_par$bg <- "white"
            default_par$bty <- "o"

            default_par$cex <- 1
            default_par$cex.axis <- 1
            default_par$cex.lab <- 1
            default_par$cex.main <- 1.2
            default_par$cex.sub <- 1
            default_par$cin <- c( 0.15, 0.20 )
            default_par$col <- "black"
            default_par$col.axis <- "black"
            default_par$col.lab <- "black"
            default_par$col.main <- "black"
            default_par$col.sub <- "black"
            default_par$cra <- c( 14.4, 19.2 )
            default_par$crt <- 0
            default_par$csi <- 0.2
            default_par$cxy <- c( 0.03779924, 0.05937655 )

            default_par$din <- c( 5.208333, 5.208333 )

            default_par$err <- 0

            default_par$family <- ""
            default_par$fg <- "black"
            default_par$fig <- c( 0, 1, 0, 1 )
            default_par$fin <- c( 5.208333, 5.208333 )
            default_par$font <- 1
            default_par$font.axis <- 1
            default_par$font.lab <- 1
            default_par$font.main <- 2
            default_par$font.sub <- 1

            default_par$lab <- c( 5, 5, 7 )
            default_par$las <- 0
            default_par$lend <- "round"
            default_par$lheight <- 1
            default_par$ljoin <- "round"
            default_par$lmitre <- 10
            default_par$lty <- "solid"
            default_par$lwd <- 1

            default_par$mai <- c( 1.02, 0.82, 0.82, 0.42 )
            default_par$mar <- c( 5.1, 4.1, 4.1, 2.1 )
            default_par$mex <- 1
            default_par$mfcol <- c( 1, 1 )
            default_par$mfg <- c( 1, 1, 1, 1 )
            default_par$mfrow <- c( 1, 1 )
            default_par$mgp <- c( 3, 1, 0 )
            default_par$mkh <- 0.001

            default_par$new <- FALSE

            default_par$oma <- c( 0, 0, 0, 0 )
            default_par$omd <- c( 0, 1, 0, 1 )
            default_par$omi <- c( 0, 0, 0, 0 )

            default_par$page <- TRUE
            default_par$pch <- 1
            default_par$pin <- c( 3.968333, 3.368333 )
            default_par$plt <- c( 0.15744, 0.91936, 0.19584, 0.84256 )
            default_par$ps <- 12
            default_par$pty <- "m"

            default_par$smo <- 1
            default_par$srt <- 0

            default_par$tck <- NA
            default_par$tcl <- -0.5

            default_par$usr <- c( 0, 1, 0, 1 )

            default_par$xaxp <- c( 0, 1, 5 )
            default_par$xaxs <- "r"
            default_par$xaxt <- "s"
            default_par$xpd <- FALSE

            default_par$yaxp <- c( 0, 1, 5 )
            default_par$yaxs <- "r"
            default_par$yaxt <- "s"
            default_par$ylbias <- 0.2

            par_to_restore <- default_par

      } else{

            par_to_restore <- previous_par

      }

      # strange behaviour when whole list given to which
      removeElementsWithWarnings <- c( which( names(par_to_restore)==c( "cin")),
                                       which( names(par_to_restore)==c( "cra" ) ),
                                       which( names(par_to_restore)==c( "csi" ) ),
                                       which( names(par_to_restore)==c( "cxy" ) ),
                                       which( names(par_to_restore)==c( "din" ) ),
                                       which( names(par_to_restore)==c( "page" ) ) )

      # reset par settings to previous par
      par( par_to_restore[ - removeElementsWithWarnings ] )

      restore_figure_region( previous_par = par_to_restore, ... )

}
