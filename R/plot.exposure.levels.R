plot.exposure.levels <- function( x, pch = 1, cex = 1.3, col = "red", ... ){
  if( .Device == "null device") stop("plot.vitd.curve must be called before plot.exposure.levels")
  N <- ncol(x[[1]])
  time <- x[[1]]
  for( i in 1:N )
    points( time[,i], x[[2]][i,], pch = pch, cex = cex, col = col )
}

