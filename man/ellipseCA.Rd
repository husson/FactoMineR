\name{ellipseCA}

\alias{ellipseCA}

\title{Draw confidence ellipses in CA}

\description{
Draw confidence ellipses in CA around rows and/or columns.
}

\usage{
ellipseCA (x, ellipse=c("col","row"), method="multinomial", nbsample=100,
    axes=c(1,2), xlim=NULL, ylim=NULL, col.row="red", col.col="blue",
	col.row.ell=col.row, col.col.ell=col.col, \dots)
}

\arguments{
  \item{x}{an object of class CA}
  \item{ellipse}{a vector of character that defines which ellipses are drawn}
  \item{method}{the method to construct ellipses (see details below)}
  \item{nbsample}{number of samples drawn to evaluate the stability of the points}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{col.row}{a color for the rows points}
  \item{col.col}{a color for columns points}
  \item{col.row.ell}{a color for the ellipses of rows points (the color "transparent" can be used if an ellipse should not be drawn)}
  \item{col.col.ell}{a color for the ellipses of columns points (the color "transparent" can be used if an ellipse should not be drawn)}
  \item{\dots}{further arguments passed to or from the plot.CA function, such as title, invisible, ...}
}

\value{
Returns the factor map with the joint plot of CA with ellipses around some elements.
}

\details{
With \code{method="multinomial"}, the table X with the active elements is taken as a reference. Then new data tables are drawn in the following way: 
N (the sum of X) values are drawn from a multinomial distribution with theoretical frequencies equals to the values in the cells divided by N.\cr

With \code{method="boot"}, the values are bootstrapped row by row: Ni (the sum of row i in the X table) values are taken in a vector with Nij equals to column j (with j varying from 1 to J).

Thus \code{nbsample} new datasets are drawn and projected as supplementary rows and/or supplementary columns. Then confidence ellipses are drawn for each elements thanks to the \code{nbsample} supplementary points.
}

\references{
Lebart, L., Morineau, A. and Piron, M. (1995) Statistique exploratoire multidimensionnelle, \emph{Dunod}.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{plot.CA}}, \code{\link{CA}}}

\examples{
data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)
## Ellipses for all the active elements
ellipseCA(res.ca)
## Ellipses around some columns only
ellipseCA(res.ca,ellipse="col",col.col.ell=c(rep("blue",2),rep("transparent",3)),
     invisible=c("row.sup","col.sup"))
}

\keyword{multivariate}
