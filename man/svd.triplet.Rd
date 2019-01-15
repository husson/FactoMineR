\name{svd.triplet}

\alias{svd.triplet}

\title{Singular Value Decomposition of a Matrix}

\description{
Compute the singular-value decomposition of a rectangular matrix with weights for rows and columns.
}

\usage{
svd.triplet(X, row.w=NULL, col.w=NULL, ncp=Inf)
}

\arguments{
  \item{X}{a data matrix}
  \item{row.w}{vector with the weights of each row (NULL by default and the weights are uniform)}
  \item{col.w}{vector with the weights of each column (NULL by default and the weights are uniform)}
  \item{ncp}{the number of components kept for the outputs}
}

\value{
\item{vs}{ a vector containing the singular values of 'x';}
\item{u}{a matrix whose columns contain the left singular vectors of 'x';}
\item{v}{a matrix whose columns contain the right singular vectors of 'x'.}
}

\seealso{ \code{\link{svd}}}

\keyword{algebra}
