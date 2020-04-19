\name{plot.power.calc}
\alias{plot.power.calc}
\title{Plotting a power.calc object}
\description{
  Plot the power between two groups as the number of participants and the relative risk vary.
  }
  \usage{
    \method{plot}{power.calc}( x, col = "hotpink", lwd = 1.5, lty = 1, 
    ylab = "power", x.legend = NULL, y.legend = NULL, 
    main.legend = "", legend.size = 1, \dots )
      
  }
  \arguments{
    \item{x}{An object of class \code{power.calc}.}
    \item{col}{Character, value for the colour of the line.}
    \item{lwd}{Numeric, a value for the line width.}
    \item{lty}{Numeric, a value for the line type.}
    \item{ylab}{A title for the y-axis.}
    \item{x.legend}{Numeric, a value for the x position of the legend. If not given, tries to default to a sensible value.}
    \item{y.legend}{Numeric, a value for the y position of the legend. If not given, tries to default to a sensible value.}
    \item{main.legend}{Character, a title for the legend.}
    \item{legend.size}{Numeric, a value for the size of the legend.}
    \item{\dots}{Additional arguments to be passed to the \code{plot} function.}
  }
