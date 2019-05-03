summary.vitd.curve <- function( object, ... ){ # have to put in if loop if treatment / placebo
  
  x <- object
  
  type <- x$type # test for type of vitamin d curve 3 - placebo/trad 4 - treatment

  if( type == "placebo" | type ==  "fixed-dose"){
    cat("\n Length of study is",(length( x$time )-1)/24,"years")
    cat("\n Number of participants is",nrow(x$curve$outp))
    cat("\n Minimum vitamin d levels of participants:\n\t",round(x$curve$min.heights,1))
    cat("\n Maximum vitamin d levels of participants:\n\t",round(x$curve$max.heights, 1))
  }else{
    cat("\n Length of study is",(length( x$time )-1)/24,"years")
    cat("\n Number of participants is",nrow(x$curve$outp))
    cat("\n Minimum vitamin d levels of participants:\n\t",round(x$curve$flatheights,1))
    cat("\n Maximum vitamin d levels of participants:\n\t",round(x$curve$max.heights, 1))
  }
}


