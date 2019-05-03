plot.power.calc <- function( x, col = "hotpink", lwd = 1.5, lty = 1,
                             ylab = "power", x.legend = NULL, y.legend = NULL,
                             main.legend = "Relative Risk", legend.size = 1, ... ){
  
  if( x$test.type == 'count' || x$test.type == 'proportions' ){
    xlab <- "n (per group)"
    mainti <- paste(x$curve.type[1], "vs", x$curve.type[2], ":", x$test.type)
  }else{
    xlab <- "n (total)" 
    mainti <- paste(x$curve.type[1],"")
  }
    
  if( is.null(x.legend) ) x.legend <- min(x$Npergroup)
  if( is.null(y.legend) ) y.legend <- 1
  
  lnrr <- length( x$RR )
  
  summaryPOWER <- vector( lnrr, mode="list" )
  
  for( i in 1:lnrr ) 
  {
    if( x$MC.rep > 1 )
    {
      if( length(x$Npergroup) > 1 )
      { 
        summaryPOWER[[i]] <- apply( x$Power[i, , ], 2, summary )
      }else{ 
        summaryPOWER[[i]] <- matrix(nrow=6,ncol=length(x$Npergroup))
        summaryPOWER[[i]][,1] <- as.vector(summary( x$Power[i,,1]))
      }
    }else{
      summaryPOWER[[i]] <- matrix(nrow=6,ncol=length(x$Npergroup))
      summaryPOWER[[i]][3,] <- x$Power[i,1,]
    }
    
  }  
    
    if( lnrr > 1 & length( x$Npergroup ) == 1 ){ 
    
      if( x$MC.rep == 1 )
      {
        
        plot( x$RR, x$Power[,1,1], type = "l", col = col, lwd = lwd, lty = lty,
              main = mainti, ylim = c(0,1), xlab = "RR", ylab = ylab )
        
      }else{
        
        # make a vector of the medians
        meds <- numeric(lnrr)
        for( i in 1:lnrr ) meds[i] <- summaryPOWER[[i]][3,1]
        
        plot( x$RR, meds, type = "l", col = col, lwd = lwd, lty = lty,
              main = mainti, ylim = c(0,1), xlab = "RR", ylab = ylab, xlim=c(min(x$RR)-.5, max(x$RR)+.5) )
        
        for( i in 1:lnrr ) boxplot( x$Power[i, ,1], add=TRUE, at=x$RR[i], xaxt="n", yaxt="n", boxfill="white", boxcol=col, outline=FALSE, medcol=col, whisklty="solid", whisklwd=1.5, whiskcol=col, staplecol=col )
        
      }
    
  }else if( lnrr > 0 & length( x$Npergroup ) > 1 ){
    
    colour <- rainbow( lnrr )
    
    plot( x$Npergroup, summaryPOWER[[1]][3,], type = "l", col = col, lwd = lwd, lty = lty,
            main = mainti, ylim = c(0,1), xlab = xlab, ylab = ylab, xaxt="n", xlim=c( min(x$Npergroup)-1, max(x$Npergroup)+1 ) )
    axis( 1, at=x$Npergroup, labels=x$Npergroup)
      
    if( x$MC.rep > 1 ) boxplot( x$Power[1, ,], use.cols=TRUE, add=TRUE, at=x$Npergroup, xaxt="n", yaxt="n", boxfill="white", boxcol=col, outline=FALSE, medcol=col, whisklty="solid", whisklwd=1.5, whiskcol=col, staplecol=col )
     
    if( lnrr > 1 )
    {
      for( i in 2:lnrr ){
        lines( x$Npergroup, summaryPOWER[[i]][3,], col = colour[i], lwd = lwd, lty = lty )
        if( x$MC.rep > 1 ) boxplot( x$Power[i, ,], use.cols=TRUE, add=TRUE, at=x$Npergroup, xaxt="n", yaxt="n", boxfill="white", boxcol=colour[i], outline=FALSE, medcol=colour[i], whisklty="solid", whisklwd=1.5, whiskcol=colour[i], staplecol=colour[i] )
      }
    }
      
    legend(x.legend, y.legend, legend = x$RR, col = c(col, colour[2:lnrr]), title = main.legend, lwd = lwd, cex = legend.size )
    
  }else if( length( x$RR ) == 1 & length( x$Npergroup ) == 1 ){
    
    if( x$MC.rep == 1 )
    {
      stop("Only one relative risk and n value- plotting is not sensible.")
    }else{
      boxplot( x$Power[1, ,], use.cols=TRUE, add=FALSE, boxfill="white", boxcol=col, outline=FALSE, medcol=col, whisklty="solid", whisklwd=1.5, whiskcol=col, staplecol=col, xlab=xlab, ylab=ylab, xaxt="n", main = mainti )
      axis(1, at=1, labels=x$Npergroup )
    }
  }
}
