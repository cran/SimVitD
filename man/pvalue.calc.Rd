\name{pvalue.calc}
\alias{pvalue.calc}
\title{Generate a pvalue for a test of difference between two groups}
\description{
  Generates a pvalue to test for a statistically significant difference between two groups.
}
\usage{
  pvalue.calc( placebo.infection, treatment.infection, test.type )
}
\arguments{
  \item{placebo.infection}{An object of class \code{infection.count}.}

  \item{treatment.infection}{An object of class \code{infection.count}.}

  \item{test.type}{Character, one of 'proportions', 'crossover', 'count'. The type of test you would like to calculate a pvalue for.}

}

\value{

  \code{\link{pvalue.calc}} returns a list with the summary from the test and the pvalue.
}

\author{Rebecca Mangan}
