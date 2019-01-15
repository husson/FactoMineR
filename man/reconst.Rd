\name{reconst}

\alias{reconst}

\title{Reconstruction of the data from the PCA, CA or MFA results}

\description{
Reconstruct the data from the PCA, CA or MFA results.\cr
}

\usage{
reconst(res, ncp=NULL)}

\arguments{
  \item{res}{an object of class PCA, CA or MFA}
  \item{ncp}{number of dimensions used to reconstitute the data (by default NULL and the number of dimensions calculated for the PCA, CA or MFA is used)}
}

\value{
Returns a data frame with the number of individuals and the number of variables used for the PCA, CA or MFA
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}, Julie Josse\email{Julie.Josse@agrocampus-ouest.fr}}

\seealso{ \code{\link{PCA}},\code{\link{CA}}, \code{\link{MFA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph=FALSE)
rec <- reconst(res.pca,ncp=2)
}

\keyword{multivariate}
