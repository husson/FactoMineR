\name{MFA}

\alias{MFA}

\title{Multiple Factor Analysis (MFA)}

\description{
Performs Multiple Factor Analysis in the sense of Escofier-Pages with supplementary individuals and supplementary groups of variables.
Groups of variables can be quantitative, categorical or contingency tables.\cr
Specific Multiple Fac tor Analysis can be performed using the argument excl.\cr
Missing values in numeric variables are replaced by the column mean.\cr
Missing values in categorical variables are treated as an additional level.
}

\usage{
MFA (base, group, type = rep("s",length(group)), excl = NULL, 
    ind.sup = NULL, ncp = 5, name.group = NULL,  
    num.group.sup = NULL, graph = TRUE, weight.col.mfa = NULL, 
    row.w = NULL, axes = c(1,2), tab.comp=NULL)
}

\arguments{
  \item{base}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (variables)}
  \item{group}{a vector with the number of variables in each group}
  \item{type}{the type of variables in each group; four possibilities: "c" or "s" for quantitative variables
  (the difference is that for "s" variables are scaled to unit variance), "n" for categorical variables and
  "f" for frequencies (from a contingency tables); by default,
  all variables are quantitative and scaled to unit variance}
  \item{excl}{an argument that may possible to exclude categories of active variables of categorical variable groups. 
  NULL by default, it is a list with indexes of categories that are excluded per group}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{name.group}{a vector containing the name of the groups (by default, NULL and the group are named group.1, group.2 and so on)}
  \item{num.group.sup}{the indexes of the illustrative groups (by default, NULL and no group are illustrative)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{weight.col.mfa}{vector of weights, useful for HMFA method (by default, NULL and an MFA is performed)}
  \item{row.w}{an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{tab.comp}{object obtained from the imputeMFA function of the missMDA package that allows to handle missing values}
}

\value{
  \item{summary.quali}{a summary of the results for the categorical variables}
  \item{summary.quanti}{a summary of the results for the quantitative variables}
  \item{separate.analyses}{the results for the separate analyses}
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{group}{a list of matrices containing all the results for the groups (Lg and RV coefficients, coordinates, square cosine, contributions, distance to the origin, the correlations between each group and each factor)}
  \item{rapport.inertie}{inertia ratio}
  \item{ind}{a list of matrices containing all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{ind.sup}{a list of matrices containing all the results for the supplementary individuals (coordinates, square cosine)}
  \item{quanti.var}{a list of matrices containing all the results for the quantitative variables (coordinates, correlation between variables and axes, contribution, cos2)}
  \item{quali.var}{a list of matrices containing all the results for categorical variables (coordinates of each categories of each variables, contribution and v.test which is a criterion with a Normal distribution)}
  \item{freq}{a list of matrices containing all the results for the frequencies (coordinates, contribution, cos2)}
  \item{quanti.var.sup}{a list of matrices containing all the results for the supplementary quantitative variables (coordinates, correlation between variables and axes, cos2)}
  \item{quali.var.sup}{a list of matrices containing all the results for the supplementary categorical variables (coordinates of each categories of each variables, cos2 and v.test which is a criterion with a Normal distribution)}
  \item{freq.sup}{a list of matrices containing all the results for the supplementary frequencies (coordinates, cos2)}
  \item{partial.axes}{a list of matrices containing all the results for the partial axes (coordinates, correlation between variables and axes, correlation between partial axes)}
  \item{global.pca}{the result of the analysis when it is considered as a unique weighted PCA}

Returns the individuals factor map, the variables factor map and the groups factor map.\cr
The plots may be improved using the argument autolab, modifying the size of the labels or selecting some elements thanks to the \code{\link{plot.MFA}} function.
}

\references{
Escofier, B. and Pages, J. (1994) Multiple Factor Analysis (AFMULT package). \emph{Computational Statistics and Data Analysis}, 18, 121-140.\cr
Becue-Bertaut, M. and Pages, J. (2008) Multiple factor analysis and clustering of a mixture of quantitative, categorical and frequency data. \emph{Computational Statistice and Data Analysis}, 52, 3255-3268.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}, J. Mazet}

\seealso{ \code{\link{print.MFA}}, \code{\link{summary.MFA}}, \code{\link{plot.MFA}}, \code{\link{dimdesc}},\cr
\href{https://www.youtube.com/watch?v=g5_hM93e8HM&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=6}{Video showing how to perform MFA with FactoMineR}}


\examples{
data(wine)
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))
summary(res)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))

\dontrun{
#### Confidence ellipses around categories per variable
plotellipses(res)
plotellipses(res,keepvar="Label") ## for 1 variable

#### Interactive graph
liste = plotMFApartial(res)
plot(res,choix="ind",habillage = "Terroir")

###Example with groups of categorical variables
data (poison)
MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
    name.group=c("desc","desc2","symptom","eat"),
    num.group.sup=1:2)

###Example with groups of frequency tables
data(mortality)
res<-MFA(mortality,group=c(9,9),type=c("f","f"),
    name.group=c("1979","2006"))

## Graphical interface
require(Factoshiny)
res <- Factoshiny(wine)

### with missing values
require(missMDA)
data(orange)
res.impute <- imputeMFA(orange, group=c(5,3), type=rep("s",2),ncp=2) 
res.mfa <- MFA(res.impute$completeObs,group=c(5,3),type=rep("s",2)) 
}
}

\keyword{multivariate}
