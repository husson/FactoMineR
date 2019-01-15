\name{print.MFA}

\alias{print.MFA}

\title{Print the Multiple Factor Analysis results}

\description{
Print the Multiple Factor Analysis results.
}

\usage{
\method{print}{MFA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class MFA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MFA}}, \code{\link{write.infile}}}

\keyword{print}
