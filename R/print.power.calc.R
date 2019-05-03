print.power.calc <- function( x, ... ){
  if( x$MC.rep == 1 )
  {
    if( length(x$Npergroup) > 1 & length(x$RR) > 1 )
    {
      A <- x$Power[,1,]
      colnames( A ) <- paste0("n=",x$Npergroup)
      rownames( A ) <- paste0("RR=",x$RR)
    }else{
      A <- x$Power[,1,]
      if( length(x$Npergroup) > 1 ) 
        names(A) <- paste0("n=",x$Npergroup)
      else
        names(A) <- paste0("RR=",x$RR)
    }
  }else{
    pr <- vector( length(x$RR), mode="list" )
    names(pr) <- paste0("RR=",x$RR)
    for( i in 1:length(x$RR) )
    {
      pr[[i]] <- apply( x$Power[i,,], 2, summary )
      colnames( pr[[i]] ) <- paste0("n=",x$Npergroup)
    }
  }
  cat( " \n Power" )
  if( x$MC.rep == 1 )
  {
    if( length(x$Npergroup) > 1 & length(x$RR) > 1 )
    {
      cat("\n")
      print( round(A, 3) )
    }else{
      if( length(x$Npergroup) > 1 ) 
      {
        cat( paste0(" @ RR=",x$RR, "\n" ) )
        print( round(A,3) )
      }else{
        cat( paste0(" @ n=",x$Npergroup, "\n")  )
        print( round(A,3) )
      }
    }
  }else{
    cat("\n")
    print( pr )
  }
  
  if( x$MC.rep > 1 ) cat( "\n Monte Carlo reps: ", x$MC.rep ) 
}
