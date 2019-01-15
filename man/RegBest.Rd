\name{RegBest}

\alias{RegBest}

\title{Select variables in multiple linear regression}

\description{
Find an optimal submodel
}

\usage{
RegBest(y,x, int = TRUE, wt=NULL, na.action = na.omit,
    method=c("r2","Cp", "adjr2"), nbest=1)
}

\arguments{
  \item{y}{A response vector}
  \item{x}{A matrix of predictors}
  \item{int}{Add an intercept to the model}
  \item{wt}{Optional weight vector}
  \item{na.action}{Handling missing values}
  \item{method}{Calculate R-squared, adjusted R-squared or Cp to select the model. By default a the F-test on the r-square is used}
  \item{nbest}{number of best models for each set of explained variables (by default 1)}
}

\value{
Returns the objects
  \item{all}{gives all the \code{nbest} best models for a given number of variables}
  \item{best}{the best model}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{lm}}}

\examples{
data(milk)
res = RegBest(y=milk[,6],x=milk[,-6])
res$best
}

\keyword{models}
