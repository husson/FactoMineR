\name{milk}

\alias{milk}

\docType{data}

\title{milk}

\description{
Dataset to illustrate the selection of variables in regression
}

\usage{data(milk)}

\format{
Dataset with 85 rows and 6 columns :
85 milks described by the 5 variables: density, fat content, protein, casein, dry, yield}

\examples{

data(milk)
res = RegBest(y=milk[,6],x=milk[,-6])
res$best
}

\keyword{datasets}
