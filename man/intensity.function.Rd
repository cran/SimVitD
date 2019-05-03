\name{intensity.function}
\alias{intensity.function}
\title{Generate an intensity function}
\description{
  Generates intensity function, function with time as sole argument, used as input to \code{nhhp.event.times} function from \code{poisson} package
}
\usage{
  intensity.function( summer.rate = 0, winter.rate = 1, flu = TRUE )
}
\arguments{
  \item{summer.rate}{Numeric value bwteeen 0 and 1, the rate at which exposures occur in summer months.}

  \item{winter.rate}{Numeric value between 0 and 1, the rate at which exposures occur in winter months.}

  \item{flu}{Logical argument. TRUE: to define winter months as flu season (Sep-May in Northern Hemisphere, Mar-Nov in Southern Hemisphere), FALSE: to have constant exposure rate all year.}

}

\value{

  \code{\link{intensity.function}} returns a function that takes
          time as sole argument and returns value between 0 and 1,
          the returned function acts as
          \code{intensity.func} for input to \code{\link{exposure.levels}}.
}


\author{Rebecca Mangan}
