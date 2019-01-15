\name{print.GPA}

\alias{print.GPA}

\title{Print the Generalized Procrustes Analysis (GPA) results}

\description{
Print the Generalized Procrustes Analysis (GPA) results.
}

\usage{
\method{print}{GPA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class GPA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Elisabeth Morand, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{GPA}}, \code{\link{write.infile}}}

\keyword{print}
