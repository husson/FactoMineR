\name{print.HCPC}

\alias{print.HCPC}

\title{Print the Hierarchical Clustering on Principal Components (HCPC) results}

\description{
Print the Hierarchical Clustering on Principal Components (HCPC) results.
}

\usage{
\method{print}{HCPC}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class HCPC}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{HCPC}}, \code{\link{write.infile}}}

\keyword{print}
