vitamin.profiles <- function( N, vitamin.d = TRUE, group.type = "Placebo", start = 0, end = 2, 
                              MinHeight = 10, MaxHeight = 80,
                              FlatHeight = 50, Spread.H = 15, 
                              Spread.A = 30, Spread.FH = 50, supp.dose = 20 ){
  
  if( isTRUE(vitamin.d) ){
    profile <- vitd.curve( N, group.type, start, end, MinHeight, MaxHeight,
                           FlatHeight, Spread.H, Spread.A, Spread.FH, supp.dose )
  }else if( !isTRUE(vitamin.d) & group.type == "Traditional" ){
    profile <- vitd.curve( N, "Treatment", start, end, 0, 0,
                           supp.dose + FlatHeight, 100, 100, Spread.FH)
  }else{
    profile <- vitd.curve( N, "Treatment", start, end, 0, 0,
                           FlatHeight, 100, 100, Spread.FH)
  }
  
  return( profile )
}
