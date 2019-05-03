print.exposure.levels <- function( x, ... ){
  # in here print time * 12/pi
  cat("\n $exposures \n")
  exp <- x$exposures * (12/pi)
  print( round( exp, 2 )  )
  cat("\n $levels \n")
  print( round( x[[2]], 2 ) )
}
