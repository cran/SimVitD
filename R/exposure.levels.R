# x is an object of class vitd.curve
exposure.levels <- function( x, rate,
                             intensity.func = intensity.function(), end = 1 ){
  
  if( class(x) != "vitd.curve" )
    stop("Argument 'x' is not of class 'vitd.curve'")
  
  N <- nrow(x[[2]][[1]]) # gives number of participants in the study
  L <- rate * ( 48 / pi ) # 48/pi is one exposure per week
  start <- x$time[1] * (pi/12)
  if( end < ( x$time[1] / 12 ) ) stop( "End time must be greater than start time" )
  
  num.exposures  <- ( end - (x$time[1] / 12) ) * rate * 50
  eventtimes <- nhpp.event.times( L, num.exposures, intensity.func, N, start )

  f1 <- function(z) # function which returns NA when the exposure times are greater than the end time
  {
    z[ z > end*pi ] <- NA
    return(z)
  }

  eventtimes <- apply( eventtimes, 2, f1 )
  
  # matrix of exposure times and vit D levels at exp times
  # only as large as greatest number of exposures
  Z <- !is.na( eventtimes )
  lngth <- apply( Z, 2, sum )

  eventtimes <- eventtimes[1:max(lngth),]
  
  type <- length(x$curve) # test for type of vitamin D curve 
  
  if( x$type == "dynamic-dose"){ # corresponds to treatment
    FH <- x$curve$flatheights
    H <- x$curve$min.heights
    A <- x$curve$max.heights - H
    eventheights <- event.height.na( FH, H, A, t(eventtimes) )
  }else if( x$type == "placebo" | x$type == "fixed-dose" ){ # corresponds to placebo / trad
    H <- x$curve$min.heights
    A <- x$curve$max.heights - H
    eventheights <- H + A * sin( t(eventtimes) ) * sin( t(eventtimes) )
  }else if( x$type == "cross-placebo-fixed-dose"){
    eventtimes.post <- eventtimes > x$cross
    H <- x$curve$min.heights
    A <- x$curve$max.heights - H 
    eventheights <- H + x$supp.dose * t(eventtimes.post) + A * sin( t(eventtimes) ) * sin( t(eventtimes) ) 
  }else if( x$type == "cross-placebo-dynamic-dose"){
    eventtimes.pre <- eventtimes <= x$cross
    eventtimes.post <- eventtimes > x$cross
    FH <- x$curve$flatheights
    H <- x$curve$min.heights
    A <- x$curve$max.heights - H
    output <- H +  A * sin( t(eventtimes) ) * sin( t(eventtimes) ) 
    Z <- output >= FH
    eventheights <- output * t( eventtimes.pre ) + ( Z * output + (1-Z) * FH ) * t( eventtimes.post )
  }
  
  if( x$type %in% c( "cross-placebo-fixed-dose", "cross-placebo-dynamic-dose" ) )
  {
    y <- list( eventtimes, eventheights, x$type, x$cross )
    names( y ) <- c("exposures", "levels", "type", "cross")
  }else{
    y <- list( eventtimes, eventheights, x$type )
    names( y ) <- c("exposures", "levels", "type")
  }
  
  class( y ) <- "exposure.levels"
  return( y )
}
