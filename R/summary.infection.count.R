summary.infection.count <- function( object, ... ){
  x <- object
  cat( "\n Baseline prevalence is", x$baseline )
  cat( "\n Relative Risk is", x$RR )
  cat( "\n Number of infections per participant is \n\t", x$count )
  cat( "\n Proportion of infected in group is \n\t",  length( which(x$count >= 1) ) / length(x$count) )
  cat( "\n Mean number of infections in group is \n\t", sum(x$count) / length(x$count) )
}

