\name{print.condes}

\alias{print.condes}

\title{Print the condes results}

\description{
Print the results of the function condes.
}

\usage{
\method{print}{condes}(x, \dots)
}

\arguments{
  \item{x}{an object of class condes}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{condes}}}

\examples{
\dontrun{
data(wine)
res <- condes(wine, num.var=3)
print(res)
}
}
\keyword{print}
