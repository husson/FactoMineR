\name{descfreq}

\alias{descfreq}

\title{Description of frequencies}

\description{
Description of the rows of a contingency table or of groups of rows of a contingency table
}

\usage{
descfreq(donnee, by.quali = NULL, proba = 0.05)}

\arguments{
  \item{donnee}{a data frame corresponding to a contingency table (quantitative data)}
  \item{by.quali}{a factor used to merge the data from different rows of the contingency table; by default NULL and each row is characterized}
  \item{proba}{the significance threshold considered to characterized the category (by default 0.05)}
}

\value{
Returns a list with the characterization of each rows or each group of the \code{by.quali}. A test corresponding to the hypergeometric distribution is performed and the probability to observe a more extreme value than the one observed is calculated. For each row (or category), each of the columns characterising the row are sorted in ascending order of p-value.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}


\seealso{ \code{\link{catdes}}, \code{\link{condes}}, \code{\link{textual}}}

\references{
Lebart, L., Morineau, A. and Piron, M. (1995) Statistique exploratoire multidimensionnelle, \emph{Dunod}.\cr
}

\examples{
data(children)
descfreq(children[1:14,1:5])    ## desc of rows
descfreq(t(children[1:14,1:5])) ## desc of columns
}

\keyword{multivariate}
