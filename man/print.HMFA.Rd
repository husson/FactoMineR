\name{print.HMFA}

\alias{print.HMFA}

\title{Print the Hierarchical Multiple Factor Analysis results}

\description{
Print the Hierarchical Multiple Factor Analysis results.
}

\usage{
\method{print}{HMFA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class HMFA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Sebastien Le, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{HMFA}}, \code{\link{write.infile}}}

\keyword{print}
