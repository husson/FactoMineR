\name{FAMD}

\alias{FAMD}

\title{Factor Analysis for Mixed Data}

\description{
FAMD is a principal component method dedicated to explore data with both continuous and categorical variables. It can be seen roughly as a mixed between PCA and MCA. More precisely, the continuous variables are scaled to unit variance and the categorical variables are transformed into a disjunctive data table (crisp coding) and then scaled using the specific scaling of MCA. This ensures to balance the influence of both continous and categorical variables in the analysis. It means that both variables are on a equal foot to determine the dimensions of variability. This method allows one to study the similarities between individuals taking into account mixed variables and to study the relationships between all the variables. It also provides graphical outputs such as the representation of the individuals, the correlation circle for the continuous variables and representations of the categories of the categorical variables, and also specific graphs to visulaize the associations between both type of variables. 
}

\usage{
FAMD (base, ncp = 5, graph = TRUE, sup.var = NULL, 
    ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.disj = NULL)
}

\arguments{
  \item{base}{a data frame with \emph{n} rows (individuals) and \emph{p} columns}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{sup.var}{a vector indicating the indexes of the supplementary variables}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{row.w}{an optional row weights (by default, uniform row weights); the weights are given only for the active individuals}
  \item{tab.disj}{object obtained from the imputeFAMD function of the missMDA package that allows to handle missing values}
}

\value{
Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{var}{a list of matrices containing all the results for the variables considered as group (coordinates, square cosine, contributions)}
  \item{ind}{a list of matrices with all the results for the individuals (coordinates, square cosine, contributions)}
  \item{quali.var}{a list of matrices with all the results for the categorical variables (coordinates, square cosine, contributions, v.test)}
  \item{quanti.var}{a list of matrices with all the results for the quantitative variables (coordinates, correlation, square cosine, contributions)}
  \item{call}{a list with some statistics}

Returns the individuals factor map.
}


\references{
Pages J. (2004). Analyse factorielle de donnees mixtes. Revue Statistique Appliquee.  LII (4). pp. 93-111.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{print.FAMD}}, \code{\link{summary.FAMD}}, \code{\link{plot.FAMD}},\cr
\href{https://www.youtube.com/watch?v=FKB96VGUgUE&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=8}{Video showing how to perform FAMD with FactoMineR}}


\examples{
\dontrun{
data(geomorphology)
res <- FAMD(geomorphology)
summary(res)

## Graphical interface
require(Factoshiny)
res <- FAMDshiny(geomorphology)

### with missing values
require(missMDA)
data(ozone)
res.impute <- imputeFAMD(ozone, ncp=3) 
res.afdm <- FAMD(ozone,tab.disj=res.impute$tab.disj) 
}
}

\keyword{multivariate}
