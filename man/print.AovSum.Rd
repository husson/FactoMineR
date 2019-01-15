\name{print.AovSum}

\alias{print.AovSum}

\title{Print the AovSum results}

\description{
Print the results of the ANOVA obtained by the function AovSum.
}

\usage{
\method{print}{AovSum}(x, \dots)
}

\arguments{
  \item{x}{an object of class AovSum}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Vincent Guyader, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{AovSum}}}

\examples{
\dontrun{
data(senso)
res <- AovSum(Score~ Product + Day , data=senso)
res
}
}
\keyword{print}
