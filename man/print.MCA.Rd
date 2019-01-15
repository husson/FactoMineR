\name{print.MCA}

\alias{print.MCA}

\title{Print the Multiple Correspondance Analysis (MCA) results}

\description{
Print the Multiple Correspondance Analysis (spMCA) results.
}

\usage{
\method{print}{MCA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class MCA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MCA}}, \code{\link{write.infile}}}

\keyword{print}
