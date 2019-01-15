\name{estim_ncp}

\alias{estim_ncp}

\title{Estimate the number of components in Principal Component Analysis}

\description{
Estimate the number of components in PCA .
}

\usage{
estim_ncp(X, ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV")
}

\arguments{
  \item{X}{a data frame with continuous variables}
  \item{ncp.min}{minimum number of dimensions to interpret, by default 0}
  \item{ncp.max}{maximum number of dimensions to interpret, by default NULL which corresponds to the number of columns minus 2}
  \item{scale}{a boolean, if TRUE (value set by default) then data are scaled to unit variance}
  \item{method}{method used to estimate the number of components, "GCV" for the generalized cross-validation approximation or "Smooth" for the smoothing method (by default "GCV")}
}

\value{
Returns ncp the best number of dimensions to use (find the minimum or the first local minimum) and the 
mean error for each dimension tested
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}, Julie Josse\email{Julie.Josse@agrocampus-ouest.fr}}

\references{Josse, J. and Husson, F. (2012). Selecting the number of components in PCA using cross-validation approximations. Computational Statistics and Data Analysis, 56, 1869-1879.
}

\seealso{ \code{\link{PCA}}}

\examples{
data(decathlon)
nb.dim <- estim_ncp(decathlon[,1:10],scale=TRUE)
}

\keyword{multivariate}
