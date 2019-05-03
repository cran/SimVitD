plot.vitd.curve <- function( x, main = " ", xlab = " ", ylab = "25 Hydroxy Vitamin D", ... ){
  time <- ( x[[1]] ) / ( 12 / pi )
  matplot( time, t(x[[2]][[1]]), type="l", main=main, xlab=xlab, ylab=ylab,
           ylim=c(0,200), axes=FALSE )
  axis( 2 )
  year <- ( length(x[[1]]) - 1 ) / (x$res * 12)
  
  if( isTRUE(x$north.hemi) ){
    months <- c( month.abb[3:12], rep(month.abb, year - 1), month.abb[1:3] )
  }else{
    months <- c( month.abb[9:12], rep(month.abb, year - 1), month.abb[1:9] )
  }
  
  axis( 1, at = seq(time[1], time[length(time)], length.out = year*12 + 1 ),
        labels = months )
}

