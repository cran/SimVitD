power.calc <- function( num.participants, num.sims, test.type, sig.level = 0.05,
                        vitdcurves.placebo = NULL, vitdcurves.treatment = NULL,
                        baseline = 0.03, RR = 3,
                        rate = 1, intensity.func = intensity.extreme, holding.time = 2, mc.error = 1, lohi.vit = c(10,70) ){
  
  placebo.group <- vitdcurves.placebo
  treatment.group <- vitdcurves.treatment
  
  if( sig.level < 0 | sig.level > 1 ) stop("Argument 'sig.level' is a significance level.")
  if( baseline < 0 ) stop("Argument 'baseline' is a probability and must be positive.")
  if( any(RR < 0 ) ) stop("Some entries of 'RR' are negative.")
  if( rate < 0 ) stop("Argument 'rate' must be positive.")
  if( holding.time < 0 ) stop("Argument 'holding.time' must be positive." )
  if( mc.error < 0 | mc.error != floor(mc.error) ) stop("Argument 'mc.error' must be a positive integer.")
  if( any( baseline*RR > 1 ) ) stop("Argument 'RR' scales 'baseline' to probabilities greater than 1.")
  if( !( test.type %in% c('crossover','proportions','count') )) stop("Argument 'test.type' must be one of 'crossover','proportions','count'.")
  if( (!is.null(placebo.group) & class(placebo.group) != 'vitd.curve') | (!is.null(treatment.group) & class(treatment.group)!= 'vitd.curve' ) ) stop("Arguments 'vitdcurves.placebo' and 'vitdcurves.treatment' must be of class 'vitd.curve'.")
  
  curve.type <- c(placebo.group$type, treatment.group$type)

  POWER <- array( dim=c( length(RR), mc.error, length(num.participants) ) )
  
  #POWER <- matrix( ncol = length(num.participants), nrow = length(RR) )

  pb <- txtProgressBar( min = 0, max = length(num.participants)*length(RR)*mc.error*num.sims, style = 3 )
  pr.counter <- 0

  for( iter in 1:length(num.participants) ){
    N <- num.participants[iter]

    for( iteration in 1:length(RR) ){
      
      for( rep in 1:mc.error )
      {
        
        power_vector <- rep( 0, num.sims )
        p <- rep( 0, num.sims )
  
        rel.risk <- RR[iteration]
  
        lower <- baseline
        upper <- lower * rel.risk
        intercept <- -4
        slope <- 0.1
  
        start <- treatment.group$time[1] / 12
        end <- treatment.group$time[length(treatment.group$time)] / 12
        cross  <- treatment.group$cross/pi
        years <- end - start
        time <- seq( start * pi, end * pi, length.out = years * 24 + 1 )
  
          
        for( dataset in 1:num.sims ){
          
            if( test.type != "crossover" )
            {
          
              placebo <- vitd.curve( N, placebo.group$type, start, end, cross, placebo.group$min.height, placebo.group$max.height,
                                     placebo.group$flatheight, placebo.group$spread.min, placebo.group$spread.max,
                                     placebo.group$spread.fh, placebo.group$supp.dose )
              placebo.levels <- exposure.levels( placebo, rate,
                                                 intensity.func = intensity.func, end )
              placebo.disease <- infection.count( placebo.levels, baseline, rel.risk, holding.time, lohi.vit=lohi.vit )
      
      
              treat <- vitd.curve( N, treatment.group$type, start, end, cross, treatment.group$min.height, treatment.group$max.height,
                                   treatment.group$flatheight, treatment.group$spread.min, treatment.group$spread.max,
                                   treatment.group$spread.fh, treatment.group$supp.dose )
              treat.levels <- exposure.levels( treat, rate,
                                               intensity.func = intensity.func, end )
              treat.disease <- infection.count( treat.levels, baseline, rel.risk, holding.time, lohi.vit=lohi.vit )
            
            }else{
              
              if( is.null( treatment.group ) ) stop("For crossover designs, only the crossover 'vitd.curve' object should be passed as argument 'vitdcurves.treatment'-- argument 'vitdcurves.placebo' is ignored.")
              #if( !is.null(placebo.group) ) warning("For crossover designs, only the crossover 'vitd.curve' object should be passed as argument 'treatment.group'-- argument 'placebo.group' will beignored.")
              
              z <- any( treatment.group$type  %in% c("cross-placebo-fixed-dose", "cross-placebo-dynamic-dose") )
              if( !z ) stop("Argument 'test.type' is equal to 'crossover' but 'treatment.group' is not a corresponding vitd.curve type.")
              
              # get the placebo curves
              treat <- vitd.curve( N, treatment.group$type, start, end, treatment.group$cross/pi, treatment.group$min.height, treatment.group$max.height,
                                   treatment.group$flatheight, treatment.group$spread.min, treatment.group$spread.max,
                                   treatment.group$spread.fh, treatment.group$supp.dose )
              treat.levels <- exposure.levels( treat, rate,
                                               intensity.func = intensity.func, end )
              treat.disease <- infection.count( treat.levels, baseline, rel.risk, holding.time, lohi.vit=lohi.vit )
              
              placebo.disease <- NULL
            }
    
            grouptest <- pvalue.calc( placebo.disease, treat.disease, test.type )
    
            p[dataset] <- grouptest$p.value
    
            if( !is.na(p[dataset]) & p[dataset] < sig.level ){
              power_vector[dataset] <- 1
            }
    
            pr.counter <- pr.counter + 1
            setTxtProgressBar(pb, pr.counter)
          }
    
          power <- length(which(power_vector == 1))/num.sims
          POWER[iteration, rep, iter] <- power
  
      }
      
    
    }
  }
  close(pb)

  y <- list( curve.type, test.type, baseline, RR, num.participants, mc.error, POWER )
  names( y ) <- c( "curve.type", "test.type", "baseline", "RR", "Npergroup", "MC.rep", "Power" )
  class( y ) <- "power.calc"
  return( y )

}
