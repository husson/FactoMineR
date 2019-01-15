\name{senso}

\alias{senso}

\docType{data}

\title{senso}

\description{
Dataset to illustrate one-way and Two-way analysis of variance
}

\usage{data(senso)}

\format{
Dataset with 45 rows and 3 columns:
Score, Product and Day}

\examples{
## Example of 2-way analysis of variance
data(senso)
res <- AovSum (Score~ Product + Day, data=senso)
res

## Example of 2-way analysis of variance with interaction
data(senso)
res2 <- AovSum (Score~ Product + Day + Product : Day, data=senso)
res2

}

\keyword{datasets}
