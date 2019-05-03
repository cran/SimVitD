summary.power.calc <- function( object, ... ){
  x <- object
  cat( "\n Groups tested are", x$curve.type[1],"and",x$curve.type[2])
  cat( "\n Test type is", x$test.type )
  cat( "\n Baseline prevalence is", x$baseline )
  cat( "\n Relative Risk is", x$RR )
}

