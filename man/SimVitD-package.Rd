\name{SimVitD-package}
\alias{SimVitD-package}
\docType{package}
\title{Simulation Tools for Vitamin D studies.}
\description{
Simulation tools for planning vitamin D studies including power estimation. Population heterogeneity in vitamin D status is modelled by simulating individual vitamin D status profiles. Indviduals are assumed to be exposed randomly to an infectious agent. Exposures are simulated at the individual level. Development of infection from exposure is radomly simulated, with the probability of infection being modulated by the individual's vitamin D status at time of exposure. Power estimates are calculated, accounting for heterogeneous treatment effects within individuals.
}

\details{ 

\tabular{ll}{
Package: \tab SimVitD\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2019-04-10\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

\code{SimVitD} assumes the study focus is on the immunoprotective benefits against some infectious agent (or protection against some event to which study participants are exposed intermittently). The core elements of the simulation involve 
  
  (i) simulation of an individual's vitamin D status profile which, without supplementation, will is  assumed sinusoidal in shape, with a cycle of 1 year and a natural trough in Winter time
  
  (ii) simulation of exposures to an infectious agent in the individual's envrionment
  
  (iii) simulation of the the immuno response (developing infection or not) having being exposed, which will be dependent on the individual's vitamin D status at time of exposure.
  
There is functionality to carry out estimation of the power of detecting differences between different supplementation schemes through simulation. This can be expanded to aid in study planning and design.

The fuctions \code{\link{vitd.curve}} and \code{\link{exposure.levels}} and the accompanying documentation provide a gateway into the simulation tools. Estimation of the power of detecting differences in treatment effects are based on microsimulation; a proposed study is simulated a large number of times and within each of these studies individual vitamin D status profiles are generated to reflect variability in the population. The package considers immunoprotective effects of vitamin D; within each study, exposures to an infectious agent are simulated for each individual. For each exposure, the immune response modulated by the individual's vitamin D status is also simulated. The resulting datasets of cases/non-cases provide the basis for empirical power estimation.
}

\examples{
# simulate individual Vitamin D profiles
indprofiles <- vitd.curve( N=10, type="placebo" )

# make a plot of all these profiles
plot( indprofiles )

# intensity function for exposures to infection
intensfun <- intensity.function( summer.rate=0.1, winter.rate=0.9 )

# exposure times
expos <- exposure.levels( indprofiles, rate=2, intensfun, end=2 )

# plot of exposures on top of Vitamin D profiles
plot( expos )

# infection calculation
infect <- infection.count( expos )

# plot infection points on top of exposure points
infection.count.plot( expos, infect )
}

\author{Rebecca Mangan, Jason Wyse, Lina Zgaga}
