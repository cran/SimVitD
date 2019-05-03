# expos is output from exposure levels
# infect is output from infection.count
rr.curve.plot <- function( expos, infect, main = NULL, xlab = "25 Hydroxy Vitamin D",
                           ylab = "RR", col = "red", pch = 1, cex = 1 ){
   
  x <- expos
  y <- infect
                          
  if( class(x) != "exposure.levels" ) stop("Argument 'expos' not of class 'exposure.levels'")
  if( class(y) != "infection.count" ) stop("Argument 'infect' not of class 'infection.count'")
  
  N <- ncol(x[[1]])
  
  lvl <- seq( 0, 150, 1 )
  
  lower <- y$baseline
  upper <- lower * y$RR
  
  lo <- y$inflection[1]
  hi <- y$inflection[2]
  
  tau <- 0.045*(hi-lo) # tau to control inflection points
  
  slope <- log( (hi-lo-tau)^2/tau^2 ) / (hi-lo)
  intercept <- log( (hi-lo-tau)/tau ) - slope * hi
  
  OR.curve <- glf( lvl, lower, upper, intercept, slope )
  
  plot( lvl, OR.curve, type = "l", xlab = xlab, ylab = ylab, axes = FALSE )
  axis( 2, at = seq( lower, upper, length.out = 11 ), labels = seq( 1, y$RR, length.out = 11 ) )
  axis( 1, at = seq( 0, 150, 1 ), labels = seq( 0, 150, 1 ) )
  if( is.null(main) ) main <- paste0( expos$type, " group" )
  title( main = main )
  
  for( i in 1:N ){
    points( x$levels[i,], y$probs[i,], pch = pch, col = col, cex = cex )
  }
  
  for( i in 1:N ){
    index <- which( y$infect[i,] == 1 )
    points( x$levels[i,index], y$probs[i,index], pch = 20, col = "blue" )
  }
  
}
