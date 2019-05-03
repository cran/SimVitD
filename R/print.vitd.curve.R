print.vitd.curve <- function( x, ...  ){
  type <- length(x[[2]])

  if( type == 3 ){
    #cat("\n Output for curves \n")
    cat("\n $curve$outp \n")
    print( round(x$curve$outp,1)  )
    cat("\n $curve$min.heights \n")
    print( round(x$curve$min.heights,1) )
    cat("\n $curve$max.heights \n")
    print( round(x$curve$max.heights,1) )
  }else{
    cat("\n $curve$outp \n")
    print( round(x$curve$outp,1)  )
    cat("\n $curve$min.heights \n")
    print( round(x$curve$min.heights,1) )
    cat("\n $curve$max.heights \n")
    print( round(x$curve$max.heights,1) )
    cat("\n $curve$flatheights \n")
    print( round(x$curve$flatheights,1) )
  }

}
