\name{write.infile}

\alias{write.infile}

\title{Print in a file}

\description{
Print in a file.
}

\usage{
write.infile(X, file, sep=";", append = FALSE, nb.dec=4)
}

\arguments{
  \item{X}{an object of class list, data.frame, matrix, ...}
  \item{file}{A connection, or a character string naming the file to print to}
  \item{sep}{character string to insert between the objects to print (if the argument file is not NULL)}
  \item{append}{logical. If TRUE output will be
          appended to file; otherwise, it will overwrite the contents
          of file.}
  \item{nb.dec}{number of decimal printed, by default 4}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\examples{
\dontrun{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
write.infile(res.pca, file="c:/essai.csv", sep = ";")
}
}
\keyword{print}
