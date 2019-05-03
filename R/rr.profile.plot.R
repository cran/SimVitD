# vitdcurves is output from vitd.curve
# expos is output from exposure levels
# infect is output from infection.count

rr.profile.plot <- function( vitdcurves, expos, infect )
{
  
  if( class(vitdcurves) != "vitd.curve" ) stop("Argument 'vitdcurves' not of class 'vitd.curve'")
  if( class(expos) != "exposure.levels" ) stop("Argument 'expos' not of class 'exposure.levels'")
  if( class(infect) != "infection.count") stop("Argument 'infect' not of class 'infection.count'")
  
  par( mfrow = c(1,2) )
  time <- ( vitdcurves$time[1:(12*vitdcurves$res+1)] ) / ( 12 / pi )
  h <- max( t ( vitdcurves$curve$outp[1,] ) ) + 10
  
  # plot vit d profile for single participant
  plot( time, t( vitdcurves$curve$outp[1,1:(12*vitdcurves$res+1)] ), type = "l", xlab = "time",
        ylab = "25 Hydroxy Vitamin D", ylim = c(0, h), axes = FALSE )
  
  axis( 2 )
  months <- c( month.abb[3:12], month.abb[1:3] )
  axis( 1, at = seq( time[1], time[length(time)], length.out = 13 ), labels = months )
  
  year <- length( which(expos$exposures[,1] < pi) )
  points( expos$exposures[1:year,1], expos$levels[1,1:year], 
         type = "p", pch = 1, cex = 1.3, col = "red" )
  
  index <- which( infect$infection[1,1:year] == 1 )
  points( expos$exposures[index,1], expos$levels[1,index], 
          type = "p", pch = 20, cex = 1.5, col = "blue" )
  
# plot relative risk curve
  lvl <- seq( 0, h - 10 , 1 )
  
  lower <- infect$baseline
  upper <- lower * infect$RR
  
  lo <- infect$inflection[1]
  hi <- infect$inflection[2]
  
  tau <- 0.045*(hi-lo) # tau to control inflection points
  
  slope <- log( (hi-lo-tau)^2/tau^2 ) / (hi-lo)
  intercept <- log( (hi-lo-tau)/tau ) - slope * hi
  
  OR.curve <- glf( lvl, lower, upper, intercept, slope )
  
  plot( OR.curve, lvl, type = "l", 
        xlab="Relative Risk", ylab="" , ylim=c(0, h), axes = FALSE )
  axis( 1, at = seq( lower, upper, length.out = 11 ), labels = seq( 1, infect$RR, length.out = 11 ) )
  
  points( infect$probs[1,1:year], expos$levels[1,1:year], 
          type = "p", pch = 1, cex = 1.3, col = "red" )
  points( infect$probs[1,index], expos$levels[1,index], 
          type = "p", pch = 20, cex = 1.5, col = "blue" )
  
  par( mfrow = c(1,1) )
  
}
