summary.exposure.levels <- function( object, ... ){
  x <- object
  # number of exposures per participants
  l <- rep( 0, ncol(x$exposures) )
  for( i in 1:ncol(x$exposures) ){
    l[i] <- length( which( !is.na( x$exposures[,i] ) ) )
  }

  cat("\n Number of exposures per participant is \n\t",l )
}

