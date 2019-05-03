vitd.curve <- function( N, type = c("placebo","fixed-dose","dynamic-dose", "cross-placebo-fixed-dose", "cross-placebo-dynamic-dose"), start = 0, end = 2, cross = .5*(start + end),  Min.Height = 10, Max.Height = 80, Flat.Height = 50, Spread.Min = 1, Spread.Max = 1, Spread.FH = 1, supp.dose = 20, north.hemi = TRUE, res = 40  ){
  
  if( !( type %in% c("placebo","fixed-dose","dynamic-dose", "cross-placebo-fixed-dose", "cross-placebo-dynamic-dose") ) )
    stop("Argument 'type' is not valid.")
  
  if( any( c(start < 0, end < 0, end <= start ) ) ) stop("Arguments 'start' and 'end' must be positive with 'end' > 'start'")
  if( any( c(Min.Height < 0, Max.Height < 0, Max.Height < Min.Height) ) ) stop("Arguments 'Min.Height' and 'Max.Height' must be positive with 'Max.Height' > 'Min.Height'")
  #if( Flat.Height < 0 | Flat.Height > Max.Height ) stop("Argument Flat.Height must be positive. Sensible values will be less than 'Max.Height'.")
  if( any( c(Spread.Min < 0, Spread.Max < 0, Spread.FH < 0) ) ) stop( "Arguments 'Spread.Min', 'Spread.Max', 'Spread.FH' must be positive." ) 
  if( supp.dose < 0 ) stop("Argument 'supp.dose' must be positive.")
  
  if ( N > 1 ){
    Amplitude <- Max.Height - Min.Height
    Height <- Min.Height
    years <- end - start
    time1 <- seq( start*pi, end*pi, length.out=years*(12*res) + 1 ) # pi is one year
    cross1 <- cross*pi
    time <- ( time1 * (12/pi) ) # gives time in terms of months
    
    if( Spread.Min > 100 || Spread.Max > 100 || Spread.FH > 100 
        || Spread.Min < 1 || Spread.Max < 1 || Spread.FH < 1 ) stop("Spread must be from 1-100")
    
    Spread.A <- 100 / Spread.Max
    Spread.H <- 100 / Spread.Min
    Spread.FH <- 100 / Spread.FH
    
    if( type == "placebo" ){
      
      y <- list( time, random.sine( N , k.A = Spread.A, k.H = Spread.H, H.0 = Height,
                                    A.0 = Amplitude, time = time1 ), Height, Max.Height,
                 Spread.Min, Spread.Max, Spread.FH, type, supp.dose, north.hemi, res )
      names( y ) <- c( "time", "curve", "min.height", "max.height", "spread.min", "spread.max",
                       "spread.fh", "type", "supp.dose", "north.hemi", "res" )
      names( y[[2]] ) <- c( "outp", "min.heights", "max.heights" )
      
    }else if( type == "fixed-dose" ){
      
      y <- list( time, random.sine( N , k.A = Spread.A, k.H = Spread.H, H.0 = supp.dose + Height,
                                    A.0 = Amplitude, time = time1 ), Height, Max.Height,
                 Spread.Min, Spread.Max, Spread.FH, type, supp.dose, north.hemi, res )
      names( y ) <- c( "time", "curve", "min.height", "max.height", "spread.min", "spread.max",
                       "spread.fh", "type", "supp.dose", "north.hemi", "res" )
      names( y[[2]] ) <- c( "outp", "min.heights", "max.heights" )
      
    }else if( type == "dynamic-dose" ){
      
      y <- list( time, random.flatsine( N, k.A = Spread.A, k.H = Spread.H,
                                        k.FH = Spread.FH, H.0 = Height,
                                        A.0 = Amplitude, FH.0 = Flat.Height, time = time1 ), Height, Max.Height,
                 Flat.Height, Spread.Min, Spread.Max, Spread.FH, type, supp.dose, north.hemi, res )
      names( y ) <- c( "time", "curve", "min.height", "max.height", "flatheight",
                       "spread.min", "spread.max", "spread.fh", "type", "supp.dose", "north.hemi", "res" )
      names( y[[2]] ) <- c( "outp", "min.heights", "max.heights", "flatheights" )
      
    }else if( type == "cross-placebo-fixed-dose" ){ 
      
      y <- list( time, random.sine( N , k.A = Spread.A, k.H = Spread.H, H.0 = Height,
                                    A.0 = Amplitude, time = time1, cross=cross1, delta=supp.dose ), Height, Max.Height,
                 Spread.Min, Spread.Max, Spread.FH, type, supp.dose, north.hemi, cross1, res )
      names( y ) <- c( "time", "curve", "min.height", "max.height", "spread.min", "spread.max",
                       "spread.fh", "type", "supp.dose", "north.hemi", "cross", "res" )
      names( y[[2]] ) <- c( "outp", "min.heights", "max.heights" )
      
    }else if( type == "cross-placebo-dynamic-dose" ){
      
      y <- list( time, random.flatsine( N, k.A = Spread.A, k.H = Spread.H,
                                        k.FH = Spread.FH, H.0 = Height,
                                        A.0 = Amplitude, FH.0 = Flat.Height, time = time1, cross=cross1 ), Height, Max.Height,
                 Flat.Height, Spread.Min, Spread.Max, Spread.FH, type, supp.dose, north.hemi, cross1, res )
      names( y ) <- c( "time", "curve", "min.height", "max.height", "flatheight",
                       "spread.min", "spread.max", "spread.fh", "type", "supp.dose", "north.hemi", "cross", "res" )
      names( y[[2]] ) <- c( "outp", "min.heights", "max.heights", "flatheights" )
      
    }
    
    class( y ) <- "vitd.curve"
    return( y )
    
  }else{ stop("Invalid number of participants: N must be > 1") } # warning or stop?
  
}
