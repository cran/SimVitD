\name{power.calc}
\alias{power.calc}
\title{Generate the power to detect the difference between two groups}
\description{
  generates a value for the power between two groups
}
\usage{
  power.calc( num.participants, num.sims, test.type, sig.level = 0.05,
                        vitdcurves.placebo, vitdcurves.treatment,
                        baseline = 0.03, RR = 3,
                        rate = 1, intensity.func = intensity.extreme, 
                        holding.time = 2, mc.error = 1, lohi.vit = c(10,70) )
}
\arguments{
  \item{num.participants}{Numeric, the number of participants in each group to calculate the power for.}

  \item{num.sims}{Numeric, the number of simulations of the study to run.}

  \item{test.type}{Character, one of 'proportions', 'count', 'crossover'. The type of test you would like to calculate the power for. If 'crossover' then \code{placebo.group} is ignored and \code{treatment.group} should contain curves constructed using one of the crossover options.}

  \item{sig.level}{Numeric between 0 and 1, the significance level used to test for a statistically significant difference between the groups.}
  
  \item{vitdcurves.placebo}{An object of class \code{vitd.curve}.}

  \item{vitdcurves.treatment}{An object of class \code{vitd.curve}.}

  \item{baseline}{Numeric, baseline prevalence of getting diseased at any exposure time. Probability of getting diseased when a participant has maximum vitamin D levels.}

  \item{RR}{Numeric, the relative risk of the study. The difference between getting diseased at highest and lowest vitamin D levels.}

  \item{rate}{Numeric, mean number of exposures per week, the rate at which exposures occur in the equivalent homogeneous Poisson process.}

  \item{intensity.func}{A function that takes time as sole argument and returns value between 0 and 1, input to nhpp function see poisson package.}

  \item{holding.time}{Numeric, the mean number of weeks for the holding time.}
  
  \item{mc.error}{Number of times to repeat the experiment at each \code{num.participants} value to explore Monte Carlo error.}
  
  \item{lohi.vit}{Inflection points of the relative risk curve used in \code{infection.count}.}
}

\details{If \code{test.type} is equal to \code{crossover} then \code{vitdcurves.placebo} is ignored and \code{vitdcurves.treatment} should contain curves constructed using one of the crossover options. If \code{test.type} is equal to \code{proportions} and \code{num.participants} is small, warning messages from \code{prop.test} may appear.}

\value{

   \code{\link{power.calc}} returns an object of class \code{power.calc} that is a list.
  The list has the following slots.

  \item{test.type}{The type of study the power has been calculated on.}

  \item{baseline}{The baseline prevalence for disease.}

  \item{RR}{The relative risk of the study.}

  \item{Npergroup}{The number of participants per group in the study.}
  
  \item{MC.rep}{Number of repetitions of experiment to approximate Monte Carlo error.}

  \item{Power}{A \code{length(RR)} x \code{mc.error} x \code{length(Npergroup)} array of estimated power values. }

}

\author{Rebecca Mangan}


\examples{
# simulate placebo group
placebo <- vitd.curve( N = 10, type = "placebo" )

# simulate treatment group
treatment <- vitd.curve( N = 10, type = "dynamic-dose" )

# intensity function for exposures to infection
intensfun <- intensity.function( summer.rate = 0, winter.rate = 1 )
  
# calculate power: example only- run for much larger num.sims
pow <- power.calc( num.participants = c(10,20,30), 
        num.sims = 10,  test.type = 'count', 
        vitdcurves.placebo = placebo, vitdcurves.treatment = treatment,
        baseline = 0.03, RR = c(2,3,4), rate = 1, intensity.func = intensfun )
            
### NOT RUN ###            
# approximate the Monte Carlo error in estimation of the power-- takes longer to run
#pow <- power.calc( num.participants = c(10,20,30), 
#         snum.sims = 100, test.type = 'count', 
#         vitdcurves.placebo = placebo, vitdcurves.treatment = treatment,
#         baseline = 0.03, RR = c(2,3,4), rate = 1, 
#         intensity.func = intensfun, mc.error = 10 )

# plot power curves
plot( pow, xlab = "n", x.legend = 10, y.legend = 1, main.legend = "Relative Risk" )
}
