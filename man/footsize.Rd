\name{footsize}

\alias{footsize}

\docType{data}

\title{footsize}

\description{
Dataset for the covariance analysis (a quantitative variable explained by quantitative (continuous) and qualitative (categorical) variables)
}

\usage{data(footsize)}

\format{
Dataset with 84 rows and 3 columns:
footsize, size and sex}

\examples{

data(footsize)
res3 <- AovSum (footsize ~ size + sex + size :sex, data=footsize)
res3
}

\keyword{datasets}
