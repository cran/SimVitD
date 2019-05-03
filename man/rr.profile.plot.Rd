\name{rr.profile.plot}
\alias{rr.profile.plot}
\title{Plotting a vitamin D profile and relative risk curve for vitamin D.}
\description{
  Plot a vitamin D status profile for a single participant and the relative risk curve for vitamin D (with exposure times and whether a participant was infected at that exposure time) side by side.}
\usage{
  rr.profile.plot( vitdcurves, expos, infect )
}
\arguments{
  \item{vitdcurves}{An object of class \code{vitd.curve}.}
  \item{expos}{An object of class \code{exposure.levels}.}
  \item{infect}{An object of class \code{infection.count}.}
}

\author{Rebecca Mangan}

\examples{
# simulate vitamin D profiles
profiles <- vitd.curve( N = 10, type = "placebo" )

# intensity function for exposures to infection
intensfun <- intensity.function( summer.rate=0.1, winter.rate=0.9 )

# simulate exposure times
expos <- exposure.levels( profiles, rate=2, intensfun, end=2 )

# simulate disease
infection <- infection.count( expos )

# plot 
rr.profile.plot( profiles, expos, infection )
}
