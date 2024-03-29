\name{plot.meansComp}

\alias{plot.meansComp}

\title{Draw the means comparisons}

\description{
Plot the graphs for the means comparisons.
}

\usage{
\method{plot}{meansComp}(x, \dots)
}

\arguments{
  \item{x}{an object of class meansComp.}
  \item{\dots}{further arguments passed to or from other methods, such as ggplot, ...}
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{\code{\link{meansComp}}}

\examples{
  data(senso)
  res <- LinearModel(Score~ Product + Day , data=senso, selection="none")
  meansComp(res,~Product)
  
\dontrun{
  ## and with the sidak correction
  meansComp(res,~Product,adjust="sidak")
}}

\keyword{dplot}
