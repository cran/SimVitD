
# Functions to be used for Vitamin D Simulation

#################################################################
# Rate Function for nhpp that has zero in summer and 1 in winter
# allows for many years

intensity.extreme <- function(t)
{
  
  intens <- numeric(length(t))
  
  intens[ t < pi/6 ] <- 1
  
  a <- ( t - pi / 6 ) / ( pi / 12 )
  
  b <- floor(a) %% 12
  
  intens[ b < 4 & t >= pi/6 ] <- 0
  
  intens[ b >= 4 & t >= pi/6 ] <- 1
  
  return( intens )

}

#################################################################
# Function that returns N random sine curves, amplitudes and heights
random.sine <- function( N = 1, k.H=5, k.A=5, H.0=5, A.0=75, time, cross=0, delta=0  )
{
  
 timemat <- matrix( rep(time, N ), ncol=length(time),  byrow=TRUE )
 
 A <- rgamma( N, shape=k.A^2, rate=k.A^2/A.0 )
 H <- rgamma( N, shape=k.H^2, rate=k.H^2/H.0 )
 
 Z <- timemat > cross
 
 outp <- H + A  * sin( timemat ) * sin( timemat ) + delta * Z
 
 return(list(outp, H, H + A))  
  
}

#################################################################
# Function that returns sine curve but returns flat line if sine value
# drops below certain threshold (Flat.Height)
flat.sine <- function(Flat.Height, Height, Amp, t, cross )
{ 
  
  nr <- length( Flat.Height )
  
  timemat <- matrix( rep(t,nr), nrow=nr, byrow=TRUE )
  
  W <- timemat > cross
  
  output <- Height + Amp * sin( timemat ) * sin( timemat )
  
  Z <- output >= Flat.Height
  
  output <- ( 1 - W ) * output + (Z * output + ( 1 - Z ) * Flat.Height ) * W
  
  return( output )
  
}

#################################################################
# Function that returns N random flat sine curves, amplitudes and heights
# input constraints for variance of variables (lower value more variance)
# input height & amp of sine curve
# input flat height
random.flatsine <- function(N = 125, k.A, k.H, k.FH, A.0, H.0, FH.0, time, cross=-1 )
{
	
	A <- rgamma( N, shape=k.A^2, rate=k.A^2/A.0 )
	H <- rgamma( N, shape=k.H^2, rate=k.H^2/H.0 )
	FH <- rgamma( N, shape=k.FH^2, rate=k.FH^2/FH.0 )
	
	FH[ FH < FH.0 ] <- FH.0

  outp <- flat.sine( FH, H, A, time, cross )

  return(list(outp, H, A + H, FH)) 
}

#################################################################
# Input event times from nhpp and returns vitamin d levels
# for flat sine curves when there are na event times
event.height.na <- function(Flat.Height, Height, Amp, t)
{

	# this can be made more efficient too
	
  output <- Height + Amp * sin(t) * sin(t)
  
  Z <- output >= Flat.Height
  
  output <- Z * output + (1 - Z) * Flat.Height
  
  return( output )

}



#################################################################
# Generate generalised logisitic regression curve

glf <- function( x, l.asym, u.asym, intercept, slope )
{
  return( l.asym + (u.asym-l.asym)/(1 + exp(intercept+slope*x)) )
}

#################################################################
# Input vector with binary disease (0, 1), vector of event times,
# rate - ouput vector with binary disease including holidaing time

holding.time <- function(disease.vec, t, rate){
  
  disease <- t( disease.vec )
  
  nr <- nrow( disease )
  nc <- ncol( disease )
  
  hold.times <- matrix( rexp( nr*nc, rate ), nrow=nr, ncol=nc )
  
  tphold <- t + hold.times
  tphold <- rbind( t[1,], tphold[1:(nr-1),] )
  
  Z <- ( t < tphold ) * disease
  
  disease <- disease - Z
  
  return( t(disease) )

}

#################################################################
# Input vector of event times from nhpp and end time of study
# output vector with NAs for event times after end of study
eventtimes.end <- function(t, end){
  
  U <- which( t > end, arr.ind = TRUE )
  
  for( k in 1:nrow(U) ) t[ U[k,1], U[k,2] ] <- NA
  
  return(t)
}

#################################################################
# Input empty matrix to fill, matrix of random uniformly distr,
# matrix of probs from OR curve
# Output matrix of (0,1) whether person got disease or not
disease_test <- function(u, prob){
  
  return( u < prob )
  
}

#################################################################
# Function that takes matrix and returns columns
# with NA after certain time

f1 <- function(z)
{
  z[ z > pi ] <- NA
  return(z)
}

f2 <- function(z)
{
  z[ z > 2*pi ] <- NA
  return(z)
}

