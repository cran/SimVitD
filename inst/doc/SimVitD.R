### R code from vignette source 'SimVitD.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: SimVitD.Rnw:75-76
###################################################
options(prompt = "R> ", continue="...")


###################################################
### code chunk number 2: SimVitD.Rnw:293-301 (eval = FALSE)
###################################################
## control <- vitd.curve( 20, type = "placebo", start = 0, end = 1, Min.Height = 10, 
## 				Max.Height = 80, Spread.Min = 10, Spread.Max = 10 )
## plot(control)
##             
## treatment <- vitd.curve( 20, type = "dynamic-dose", start = 0, end = 1,
## 				Min.Height = 10, Max.Height = 80, Flat.Height = 50, 
## 				Spread.Min = 10, Spread.Max = 10, Spread.FH = 10 )
## plot(treatment)


###################################################
### code chunk number 3: SimVitD.Rnw:321-332 (eval = FALSE)
###################################################
## intensfun <- intensity.function( summer.rate = 0, winter.rate = 1, flu = TRUE )
## 
## control_expos <- exposure.levels( control, rate = 1, intensfun, end = 1 )
## 
## treatment_expos <- exposure.levels( treatment, rate = 1, intensfun, end = 1 )
## 
## control_inf <- infection.count( control_expos, baseline = 0.03, 
## 					RR = 3, holding.time = 2 )
##             
## treatment_inf <- infection.count( treatment_expos, baseline = 0.03, 
## 					RR = 3, holding.time = 2 )


###################################################
### code chunk number 4: SimVitD.Rnw:337-343 (eval = FALSE)
###################################################
## plot(control)
## plot(control_expos)
## infection.count.plot(control_expos, control_inf)
## plot(treatment)
## plot(treatment_expos)
## infection.count.plot(treatment_expos, treatment_inf)


###################################################
### code chunk number 5: SimVitD.Rnw:350-352 (eval = FALSE)
###################################################
## rr.curve.plot(control_expos, control_inf )
## rr.curve.plot(treatment_expos, treatment_inf )


###################################################
### code chunk number 6: SimVitD.Rnw:372-374 (eval = FALSE)
###################################################
## rr.profile.plot( control, control_expos, control_inf )
## rr.profile.plot( treatment, treatment_expos, treatment_inf )


###################################################
### code chunk number 7: SimVitD.Rnw:393-402 (eval = FALSE)
###################################################
## pow <- power.calc( num.participants = c(20,40,60), num.sims = 500, 
##         test.type = "count", sig.level = 0.05, 
##         vitdcurves.placebo = control, vitdcurves.treatment  = treatment, 
##         baseline = 0.03, RR = c(2,3,4), rate = 1, intensity.func = intensfun, holding.time = 2 )
##         
## plot( pow, x.legend = 20, y.legend = 1,
##         main.legend = "Relative Risk", legend.size = 0.8 )
##         
## abline( h = 0.8, lty = 2 )


###################################################
### code chunk number 8: SimVitD.Rnw:421-428 (eval = FALSE)
###################################################
## pow_mc <- power.calc( num.participants = c(20,40,60), num.sims = 500,                 
## 	test.type = "count", sig.level = 0.05, 
## 	vitdcurves.placebo = control, vitdcurves.treatment = treatment, 
## 	baseline = 0.03, RR = c(2,3,4), rate = 1, 
## 	intensity.func = intensfun, holding.time = 2, mc.error=10 )
## 	
## plot(pow_mc)


###################################################
### code chunk number 9: SimVitD.Rnw:441-447 (eval = FALSE)
###################################################
## crossover <- vitd.curve( 20, type = "cross-placebo-dynamic-dose", 
##               		start = 0, end = 2, cross = 1, 
##              	 	Max.Height = 80, Min.Height = 10, Flat.Height = 50,
##               		Spread.Min = 10, Spread.Max = 10, Spread.FH = 20 )
## 
## plot(crossover)


###################################################
### code chunk number 10: SimVitD.Rnw:459-466 (eval = FALSE)
###################################################
## pow_cross <- power.calc( num.participants = c(20,40,60), num.sims = 500, 
##                    test.type = "crossover", sig.level = 0.05, 
##                    vitdcurves.treatment  = crossover, 
##                    baseline = 0.03, RR = c(2,3,4), rate = 1, 
##                    intensity.func = intensfun, holding.time = 2, mc.error=10 )
## 
## plot(pow_cross)


