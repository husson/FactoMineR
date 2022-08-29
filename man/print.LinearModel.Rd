\name{print.LinearModel}

\alias{print.LinearModel}

\title{Print the LinearModel results}

\description{
Print the results of the ANOVA obtained by the function LinearModel.
}

\usage{
\method{print}{LinearModel}(x, \dots)
}

\arguments{
  \item{x}{an object of class LinearModel}
  \item{\dots}{further arguments passed to or from other methods}
}

\details{
Gives the results of the LinearModel function. If a model selection is performed, the global F-test
for the complete model is first given, then all the results are given for the selected model
(global F-test, the F-tests for main effects and interaction, the t-tests)
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{LinearModel}}}

\examples{
\dontrun{
data(senso)
res <- LinearModel(Score~ Product + Day , data=senso)
res

res2 <- LinearModel(Score~ Product + Day , data=senso, selection="BIC")
res2
}
}
\keyword{print}
