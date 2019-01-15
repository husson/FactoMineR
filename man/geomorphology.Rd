\name{geomorphology}

\alias{geomorphology}

\docType{data}

\title{geomorphology(data)}

\description{
The data used here concern a geomorphology analysis.
}

\usage{data(geomorphology)}

\format{
A data frame with 75 rows and 11 columns. Rows represent the individuals,
columns represent the different questions. 10 variables are quantitative and one variable is qualitative.
The dataset is analysed in: http://www.sciencedirect.com/science/article/pii/S0169555X11006362
}

\examples{
\dontrun{
data(geomorphology)
res <- FAMD(geomorphology)
plot(res,choix="ind",habillage=4)
}
}
\keyword{datasets}
