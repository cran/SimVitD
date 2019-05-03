\name{vitd.curve}
\alias{vitd.curve}
\title{Generate Vitamin D curves}
\description{
  Generates a vitamin D status profile curve for each individual in a group
}
\usage{
  vitd.curve( N, type = c("placebo","fixed-dose","dynamic-dose", 
              "cross-placebo-fixed-dose", "cross-placebo-dynamic-dose"), 
              start = 0, end = 2, cross = 0.5*(start + end), Min.Height = 10, 
              Max.Height = 80, Flat.Height = 50, Spread.Min = 1, 
              Spread.Max = 1, Spread.FH = 1, supp.dose = 20, north.hemi = TRUE, res = 40 )
}
\arguments{
  \item{N}{Integer, number of participants in the group.}

  \item{type}{Character, type of group, one of 'placebo', 'fixed-dose', 'dynamic-dose', 'cross-placebo-fixed-dose', 'cross-placebo-dynamic-dose'.}

  \item{start}{Integer, when the study started.}

  \item{end}{Integer, when the study ended.}
  
  \item{cross}{For crossover design, the fraction of the study before crossing over from \code{[start,end]} }

  \item{Min.Height}{Numeric, mean minimum height of curves.}

  \item{Max.Height}{Numeric, mean maximum height of curves.}

  \item{Flat.Height}{Numeric, threshold height of curves when treatment group.}

  \item{Spread.Min}{Numeric, how the minimum heights of participants are spread around the mean. 1 corresponds to narrowest spread, 100 corresponds to most scattered spread.}

  \item{Spread.Max}{Numeric, how the maximum heights of participants are spread around the mean. 1 corresponds to narrowest spread, 100 corresponds to most scattered spread.}

  \item{Spread.FH}{Numeric, how the flat heights of participants are spread around the mean. 1 corresponds to narrowest spread, 100 corresponds to most scattered spread.}

  \item{supp.dose}{Numeric, fixed dosage administered in the traditional supplementation scheme.}
  
  \item{north.hemi}{Logical argument. TRUE: to define summer/winter months as in the Northern Hemisphere. FALSE: to define summer/winter months as in the Southern Hemisphere.}
  
  \item{res}{Resolution parameter for plotting of vitamin D curves.}
  
}

\value{

  \code{\link{vitd.curve}} returns an object of class \code{vitd.curve} that is a list.
  The list has the following slots.

  \item{time}{The values of time in the study passed to \code{plot.vitd.curve} and \code{exposure.levels}.}

  \item{curve}{A list for input to \code{plot.vitd.curve} and \code{exposure.levels} including the following slots.}
    \item{outp}{Points to plot each individual's vitamin D curve.}
    \item{min.heights}{The minimum height for each individual's curve.}
    \item{max.heights}{The maximum height for each individual's curve.}
    \item{flatheights}{The threshold level for each individual. Only returned if Treatment type.}
  
  \item{min.height}{The mean minimum height for the group. Used as input to \code{power.calc}.}

  \item{max.height}{The mean amplitude for the group. Used as input to \code{power.calc}.}

  \item{flatheight}{The mean threshold level for the group. Used as input to \code{power.calc}.}

  \item{spread.min}{The group's spread around the minimum height. Used as input to \code{power.calc}.}

  \item{spread.max}{The group's spread around the maximum height. Used as input to \code{power.calc}.}

  \item{spread.fh}{The group's spread around the threshold level. Used as input to \code{power.calc}.}

  \item{type}{The type of group. One of 'Placebo', 'Traditional', 'Treatment'. Used as input to \code{power.calc}.}
  
  \item{supp.dose}{The fixed supplementation dose when traditional supplementation group.}
  
  \item{north.hemi}{Flag for which hemisphere the study is taking place in.}
}

\author{Rebecca Mangan}

