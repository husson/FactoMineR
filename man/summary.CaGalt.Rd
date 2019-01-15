\name{summary.CaGalt}

\alias{summary.CaGalt}

\title{Printing summaries of CaGalt objects}

\description{
Printing summaries of Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) objects
}

\usage{
\method{summary}{CaGalt}(object, nb.dec = 3, nbelements=10, nbind = nbelements, 
  ncp = 3, align.names=TRUE, file="", \ldots)
}

\arguments{
  \item{object}{an object of class CaGalt}
  \item{nb.dec}{number of printed decimals}
  \item{nbelements}{number of written elements (variables, categories, frequencies); use nbelements = Inf if you want to have all the elements}
  \item{nbind}{number of written elements; use nbind = Inf to have the results for all the individuals and nbind = 0 if you do not want the results for individuals}
  \item{ncp}{number of printed dimensions}
  \item{align.names}{boolean, if TRUE the names of the objects are written using the same number of characters}
  \item{file}{a connection, or a character string naming the file to print to}
  \item{...}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\author{Belchin Kostov \email{badriyan@clinic.ub.es}, Monica Becue-Bertaut, Francois Husson}

\seealso{ \code{\link{CaGalt}}}

\examples{

\dontrun{
data(health)
res.cagalt<-CaGalt(Y=health[,1:115],X=health[,116:118],type="n")
summary(res.cagalt)
}
}

\keyword{multivariate}
