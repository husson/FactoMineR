\name{CA}

\alias{CA}

\title{Correspondence Analysis (CA)}

\description{
Performs Correspondence Analysis (CA) including supplementary row and/or column points.
}

\usage{
CA(X, ncp = 5, row.sup = NULL, col.sup = NULL, 
    quanti.sup=NULL, quali.sup = NULL, graph = TRUE, 
	axes = c(1,2), row.w = NULL, excl=NULL)}

\arguments{
  \item{X}{a data frame or a table with \emph{n} rows and \emph{p} columns, i.e. a contingency table}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{row.sup}{a vector indicating the indexes of the supplementary rows}
  \item{col.sup}{a vector indicating the indexes of the supplementary columns}
  \item{quanti.sup}{a vector indicating the indexes of the supplementary continuous variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{row.w}{an optional row weights (by default, a vector of 1 and each row has a weight equals to its margin); the weights are given only for the active rows}
  \item{excl}{numeric vector indicating the indexes of the "junk" columns (default is NULL). Useful for MCA with excl argument.}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{col}{a list of matrices with all the results for the column variable (coordinates, square cosine, contributions, inertia)}
  \item{row}{a list of matrices with all the results for the row variable (coordinates, square cosine, contributions, inertia)}
  \item{col.sup}{a list of matrices containing all the results for the supplementary column points (coordinates, square cosine)}
  \item{row.sup}{a list of matrices containing all the results for the supplementary row points (coordinates, square cosine)}
  \item{quanti.sup}{if quanti.sup is not NULL, a matrix containing the results for the supplementary continuous variables (coordinates, square cosine)}
  \item{quali.sup}{if quali.sup is not NULL, a list of matrices with all the results for the supplementary categorical variables (coordinates of each categories of each variables, v.test which is a criterion with a Normal distribution, square correlation ratio)}
  \item{call}{a list with some statistics}

Returns the row and column points factor map.\cr
The plot may be improved using the argument autolab, modifying the size of the labels or selecting some elements thanks to the \code{\link{plot.CA}} function.
}

\references{
Benzecri, J.-P. (1992) \emph{Correspondence Analysis Handbook}, New-York : Dekker \cr
Benzecri, J.-P. (1980) \emph{L'analyse des donnees tome 2 : l'analyse des correspondances}, Paris : Bordas \cr
Greenacre, M.J. (1993) \emph{Correspondence Analysis in Practice}, London : Academic Press\cr
Husson, F., Le, S. and Pages, J. (2009). Analyse de donnees avec R, \emph{Presses Universitaires de Rennes}.\cr
Husson, F., Le, S. and Pages, J. (2010). Exploratory Multivariate Analysis by Example Using R, \emph{Chapman and Hall}.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr},Jeremy Mazet}

\seealso{ \code{\link{print.CA}}, \code{\link{summary.CA}}, \code{\link{ellipseCA}}, \code{\link{plot.CA}}, \code{\link{dimdesc}},\cr
\href{https://www.youtube.com/watch?v=Y-bXsUNX5hA&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=4}{Video showing how to perform CA with FactoMineR}}

\examples{
data(children)
res.ca <- CA (children, row.sup = 15:18, col.sup = 6:8)
summary(res.ca)
## Ellipses for all the active elements
ellipseCA(res.ca)
## Ellipses around some columns only
ellipseCA(res.ca,ellipse="col",col.col.ell=c(rep("blue",2),rep("transparent",3)),
     invisible=c("row.sup","col.sup"))

\dontrun{
## Graphical interface
require(Factoshiny)
res <- CAshiny(children)
}
}

\keyword{multivariate}
