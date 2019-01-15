\name{JO}
\alias{JO}
\docType{data}
\title{Number of medals in athletism during olympic games per country}
\description{
This data frame is a contengency table with the athletism events (in row) and the coutries (in columns). Each cell gives the number of medals obtained during the 5 olympis games from 1992 to 2008 (Barcelona 1992, Atlanta 1996, Sydney 2000, Athens 2004, Beijing 2008).
}
\usage{data(JO)}
\format{
A data frame with the 24 events in athletism and in colum the 58 countries who obtained at least on medal
}
\examples{
\dontrun{
data(JO)
res.ca <- CA(JO)
res.ca <- CA(JO, axes = 3:4)
}
}
\keyword{datasets}
