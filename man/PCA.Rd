\name{PCA}

\alias{PCA}

\title{Principal Component Analysis (PCA)}

\description{
Performs Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.\cr
Missing values are replaced by the column mean.
}

\usage{
PCA(X, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
    quanti.sup = NULL, quali.sup = NULL, row.w = NULL, 
    col.w = NULL, graph = TRUE, axes = c(1,2))}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables)}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{scale.unit}{a boolean, if TRUE (value set by default) then data are scaled to unit variance}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{row.w}{an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals}
  \item{col.w}{an optional column weights (by default, uniform column weights); the weights are given only for the active variables}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{axes}{a length 2 vector specifying the components to plot}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{var}{a list of matrices containing all the results for the active variables (coordinates, correlation between variables and axes, square cosine, contributions)}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.sup}{a list of matrices containing all the results for the supplementary quantitative variables (coordinates, correlation between variables and axes)}
  \item{quali.sup}{a list of matrices containing all the results for the supplementary categorical variables (coordinates of each categories of each variables, v.test which is a criterion with a Normal distribution, and eta2 which is the square correlation corefficient between a qualitative variable and a dimension)}

Returns the individuals factor map and the variables factor map.\cr
The plots may be improved using the argument autolab, modifying the size of the labels or selecting some elements thanks to the \code{\link{plot.PCA}} function.
}

\references{
Husson, F., Le, S. and Pages, J. (2010). Exploratory Multivariate Analysis by Example Using R, \emph{Chapman and Hall}.
}
\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}, Jeremy Mazet}

\seealso{ \code{\link{print.PCA}}, \code{\link{summary.PCA}}, \code{\link{plot.PCA}}, \code{\link{dimdesc}},\cr
\href{https://www.youtube.com/watch?v=CTSbxU6KLbM&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=3}{Video showing how to perform PCA with FactoMineR}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
## plot of the eigenvalues
## barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(res.pca)
plot(res.pca,choix="ind",habillage=13)
dimdesc(res.pca, axes = 1:2)
## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(res.pca,13)

\dontrun{
## Graphical interface
require(Factoshiny)
res <- Factoshiny(decathlon)

## Example with missing data
## use package missMDA
require(missMDA)
data(orange)
nb <- estim_ncpPCA(orange,ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=50)
imputed <- imputePCA(orange,ncp=nb$ncp)
res.pca <- PCA(imputed$completeObs)
}
}

\keyword{multivariate}
