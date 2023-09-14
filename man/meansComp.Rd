\name{meansComp}

\alias{meansComp}

\title{Perform pairwise means comparisons}

\description{
Perform means comparisons and give groups of means that are not significantly different.
}

\usage{
meansComp(object, spec, graph=TRUE, ...) 
    }

\arguments{
\item{object}{A fitted model object that is supported, such as the result of a call to LinearModel, lm or aov.}
\item{spec}{A character vector specifying the names of the predictors over which means comparisons are desired. specs may also be a formula or a list (optionally named) of valid specs. Use of formulas is described in the Overview section below.}
\item{graph}{Boolean; plot the graph to compare the means.}
\item{\dots}{other arguments, cf the function \code{emmeans}.}
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{plot.meansComp}}}

\examples{
  data(senso)
  res <- LinearModel(Score~ Product + Day , data=senso, selection="none")
  meansComp(res,spec="Product")
  
\dontrun{
  ## or equivalently
  meansComp(res, ~Product)

  ## and with the sidak correction
  meansComp(res,~Product,adjust="sidak")
}}

\keyword{models}
