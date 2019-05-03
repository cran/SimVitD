# expos output from exposure.levels
# infect output from infection.count
infection.count.plot <- function( expos, infect, pch = 20, cex = 1.5, col = "blue" ){
  x <- expos
  y <- infect
  if( class(x) != "exposure.levels" ) stop("Argument x is not of class 'exposure.levels'")
  if( class(y) != "infection.count" ) stop("Argument y is not of class 'infection.count'") 
  N <- ncol(x[[1]]) # number of participants
  time <- x[[1]] #/ (12/pi)
  for( i in 1:N ){
    index <- which( y$infection[i,] == 1 ) # vector of times when each participant was diseased
    points( time[index,i], x[[2]][i,index], pch = pch, cex = cex, col = col )
  }
}

