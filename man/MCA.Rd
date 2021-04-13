\name{MCA}

\alias{MCA}

\title{Multiple Correspondence Analysis (MCA)}

\description{
Performs Multiple Correspondence Analysis (MCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.\cr
Performs also Specific Multiple Correspondence Analysis with supplementary categories and supplementary categorical variables.\cr
Missing values are treated as an additional level, categories which are rare can be ventilated
}

\usage{
MCA(X, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
    quali.sup = NULL, excl=NULL, graph = TRUE, 
	level.ventil = 0, axes = c(1,2), row.w = NULL, 
	method="Indicator", na.method="NA", tab.disj=NULL)}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (categorical variables)}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{excl}{vector indicating the indexes of the "junk" categories (default is NULL), it can be a vector of the names of the categories or a vector of the indexes in the disjunctive data table}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{level.ventil}{a proportion corresponding to the level under which the category is ventilated; by default, 0 and no ventilation is done}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{row.w}{an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals}
  \item{method}{a string corresponding to the name of the method used: "Indicator" (by default) is the CA on the Indicator matrix, "Burt" is the CA on the Burt table. For Burt and the Indicator, the graph of the individuals and the graph of the categories are given}
  \item{na.method}{a string corresponding to the name of the method used if there are missing values; available methods are "NA" or "Average" (by default, "NA")}
  \item{tab.disj}{optional data.frame corresponding to the disjunctive table used for the analysis; it corresponds to a disjunctive table obtained from imputation method (see package missMDA).}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{var}{a list of matrices containing all the results for the active variables (coordinates, square cosine, contributions, v.test, square correlation ratio)}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.sup}{a matrix containing the coordinates of the supplementary quantitative variables (the correlation between a variable and an axis is equal to the variable coordinate on the axis)}
  \item{quali.sup}{a list of matrices with all the results for the supplementary categorical variables (coordinates of each categories of each variables, square cosine and v.test which is a criterion with a Normal distribution, square correlation ratio)}
  \item{call}{a list with some statistics}

Returns the graphs of the individuals and categories and the graph with the variables.\cr
The plots may be improved using the argument autolab, modifying the size of the labels or selecting some elements thanks to the \code{\link{plot.MCA}} function.
}

\references{
Husson, F., Le, S. and Pages, J. (2010). Exploratory Multivariate Analysis by Example Using R, \emph{Chapman and Hall}.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}, Julie Josse, Jeremy Mazet}

\seealso{ \code{\link{plotellipses}}, \code{\link{summary.MCA}},\code{\link{print.MCA}}, \code{\link{plot.MCA}}, \code{\link{dimdesc}},\cr
\href{https://www.youtube.com/watch?v=reG8Y9ZgcaQ&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=5}{Video showing how to perform MCA with FactoMineR}}


\examples{
\dontrun{
## Tea example
 data(tea)
 res.mca <- MCA(tea,quanti.sup=19,quali.sup=20:36)
 summary(res.mca)
 plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
 plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
 plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
 dimdesc(res.mca)
 plotellipses(res.mca,keepvar=1:4)
 plotellipses(res.mca,keepvar="Tea")

## Hobbies example
data(hobbies)
res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)
plot(res.mca,invisible=c("ind","quali.sup"),hab="quali") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none") 
plot(res.mca,invisible=c("ind","var"),hab="quali")
dimdesc(res.mca)
plotellipses(res.mca,keepvar=1:4)

## Specific MCA: some categories are supplementary
data (poison)
res <- MCA (poison[,3:8],excl=c(1,3))

## Graphical interface
require(Factoshiny)
res <- Factoshiny(tea)

## Example with missing values : use the missMDA package
require(missMDA)
data(vnf)
completed <- imputeMCA(vnf,ncp=2)
res.mca <- MCA(vnf,tab.disj=completed$tab.disj)
}
}
\keyword{multivariate}
