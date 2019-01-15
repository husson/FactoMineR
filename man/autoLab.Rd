\name{autoLab}

\alias{autoLab}

\title{Function to better position the labels on the graphs}

\description{
Function to better position the labels on the graphs.
}

\usage{
autoLab(x, y = NULL, labels = seq(along = x), cex = 1,
                       method = c("SANN", "GA"),
                       allowSmallOverlap = FALSE,
                       trace = FALSE, shadotext = FALSE,
                       doPlot = TRUE, \dots)
}

\arguments{
  \item{x}{the x-coordinates}
  \item{y}{the y-coordinates}
  \item{labels}{the labels}
  \item{cex}{cex}
  \item{method}{not used}
  \item{allowSmallOverlap}{boolean}
  \item{trace}{boolean}
  \item{shadotext}{boolean}
  \item{doPlot}{boolean}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
See the text function
}

\keyword{dplot}
