\name{print.catdes}

\alias{print.catdes}

\title{Print the catdes results}

\description{
Print the results of the function catdes.
}

\usage{
\method{print}{catdes}(x, \dots)
}

\arguments{
  \item{x}{an object of class catdes}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Vincent Guyader, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{catdes}}}

\examples{
\dontrun{
data(wine)
res <- catdes(wine, num.var=2)
print(res)
}
}
\keyword{print}
