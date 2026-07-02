\name{plot.meansComp}

\alias{plot.meansComp}

\title{Draw the means comparisons}

\description{
Plot the graphs for the means comparisons.
}

\usage{
\method{plot}{meansComp}(x, theme = "theme_factominer", \dots)
}

\arguments{
  \item{x}{an object of class meansComp.}
  \item{theme}{a character string corresponding to a theme you want to use for the graph, by default 'theme_factominer'. The theme 'ggplot2::theme_bw()' for instance are also possible.}
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
