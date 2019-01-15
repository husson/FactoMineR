\name{print.PCA}

\alias{print.PCA}

\title{Print the Principal Component Analysis (PCA) results}

\description{
Print the Principal Component Analysis (PCA) results.
}

\usage{
\method{print}{PCA}(x, file = NULL, sep = ";", \dots)
}

\arguments{
  \item{x}{an object of class PCA}
  \item{file}{A connection, or a character string naming the file to print to.  If NULL (the default), the results are not printed in a file}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL}
  \item{\dots}{further arguments passed to or from other methods}
}

\author{Jeremy Mazet, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{PCA}}, \code{\link{write.infile}}}

\examples{
\dontrun{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
print(res.pca, file="c:/essai.csv", sep = ";")
}
}
\keyword{print}
