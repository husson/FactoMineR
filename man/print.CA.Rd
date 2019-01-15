\name{print.CA}

\alias{print.CA}

\title{Print the Correspondance Analysis (CA) results}

\description{
Print the Correspondance Analysis (CA) results.
}

\usage{
\method{print}{CA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class CA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{CA}}, \code{\link{write.infile}}}

\keyword{print}
