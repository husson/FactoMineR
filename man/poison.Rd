\name{poison}

\alias{poison}

\docType{data}

\title{Poison}

\description{
The data used here refer to a survey carried out on a sample of children of primary school
who suffered from food poisoning. They were asked about their symptoms and about what they ate.
}

\usage{data(poison)}

\format{
A data frame with 55 rows and 15 columns.
}

\examples{
data(poison)
res.mca <- MCA(poison, quanti.sup = 1:2, quali.sup=c(3,4))
}

\keyword{datasets}
