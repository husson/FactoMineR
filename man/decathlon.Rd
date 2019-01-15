\name{decathlon}

\alias{decathlon}

\docType{data}

\title{Performance in decathlon (data)}

\description{
The data used here refer to athletes' performance during two sporting events.
}

\usage{data(decathlon)}

\format{
A data frame with 41 rows and 13 columns: the first ten columns corresponds to 
the performance of the athletes for the 10 events of the decathlon. 
The columns 11 and 12 correspond respectively to the rank and the points obtained. 
The last column is a categorical variable
corresponding to the sporting event (2004 Olympic Game or 2004 Decastar)
}

\source{
Department of statistics and computer science, Agrocampus Rennes
}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
}

\keyword{datasets}
