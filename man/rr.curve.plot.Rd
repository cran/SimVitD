\name{rr.curve.plot}
\alias{rr.curve.plot}
\title{Plotting the relative risk curve for vitamin D status.}
\description{
  Plot the relative risk curve for vitamin D with exposure points and whether a participant developed infection at that exposure time.}
  \usage{
    rr.curve.plot( expos, infect, main = NULL, xlab = "25 Hydroxy Vitamin D",
                        ylab = "RR", col = "red", pch = 1, cex = 1 )
  }
  \arguments{
    \item{expos}{An object of class \code{exposure.levels}.}
    \item{infect}{An object of class \code{infection.count}.}
    \item{main}{Main title for the plot.}
    \item{xlab}{A title for the x-axis.}
    \item{ylab}{A title for the y-axis.}
    \item{col}{Character, value for the colour of the points.}
    \item{pch}{Integer, specifying symbol for the points.}
    \item{cex}{Numeric, value giving the amount by which plotting the points should be magnified.}

  }
