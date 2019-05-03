# have to input infection.count output from two curves
pvalue.calc <- function( placebo.infection = NULL, treatment.infection=NULL, test.type ){
  
  if( class(treatment.infection) != "infection.count" )
    stop("Argument 'treatment.infection' must be of class 'infection.count'")
  
  if( !is.null(placebo.infection) & class(placebo.infection) != "infection.count" )
    stop("Argument 'placebo.infection' must be of class 'infection.count'")
  
  # test.type = proportions, crossover, poisson
  N.placebo <- length( placebo.infection$count )
  N.treat <- length( treatment.infection$count )

  if( test.type == 'crossover'){
    
     test.mat <- matrix( nrow = N.treat, ncol = 3 )
     test.mat[,1] <- treatment.infection$count[,1]
     test.mat[,2] <- treatment.infection$count[,2]
     test.mat[,3] <- test.mat[,1] - test.mat[,2]

     if( length( which(test.mat[,3] != 0) ) == 0  )
     {
        # in this case you will not reject null
        p <- 1
        test <- list()
     }else{
       test <- binom.test( x = length( which( test.mat[,3] > 0 ) ),
                           n = length( which( test.mat[,3] != 0 ) ), alternative = "greater" )
       
       p <- test$p.value
     }

  }else if( test.type == 'proportions'){
    
    d.placebo <- length( which(placebo.infection$count >= 1) )
    d.treatment <- length( which(treatment.infection$count >= 1) )

    test <- prop.test( x = c(d.placebo, d.treatment), n = c(N.placebo, N.treat), 
                        alternative = "greater" )
    p <- test$p.value

  }else if( test.type == 'count' ){
    
    test <- wilcox.test( placebo.infection$count, treatment.infection$count, alternative="greater", exact=FALSE )
    
    p <- test$p.value

  }else{
    stop( "Invalid test type: test.type must be one of 'crossover', 'proportions', 'count'" )
  }

  y <- list( test, p )
  names( y ) <- c( 'test', 'p.value' )
  return( y )
}
