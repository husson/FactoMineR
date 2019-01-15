\name{DMFA}

\alias{DMFA}

\title{Dual Multiple Factor Analysis (DMFA)}

\description{
Performs Dual Multiple Factor Analysis (DMFA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.
}

\usage{
DMFA(don, num.fact = ncol(don), scale.unit = TRUE, ncp = 5, 
    quanti.sup = NULL, quali.sup = NULL, graph = TRUE, axes=c(1,2))}

\arguments{
  \item{don}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables)}
  \item{num.fact}{the number of the categorical variable which allows to make the group of individuals}
  \item{scale.unit}{a boolean, if TRUE (value set by default) then data are scaled to unit variance}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
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
  \item{quali.sup}{a list of matrices containing all the results for the supplementary categorical variables (coordinates of each categories of each variables, and v.test which is a criterion with a Normal distribution)}
  \item{svd}{the result of the singular value decomposition}
  \item{var.partiel}{a list with the partial coordinate of the variables for each group}
  \item{cor.dim.gr}{}
  \item{Xc}{a list with the data centered by group}
  \item{group}{a list with the results for the groups (cordinate, normalized coordinates, cos2)}
  \item{Cov }{a list with the covariance matrices for each group}

Returns the individuals factor map and the variables factor map.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{plot.DMFA}}, \code{\link{dimdesc}}}

\examples{
## Example with the famous Fisher's iris data
res.dmfa = DMFA ( iris, num.fact = 5)
}

\keyword{multivariate}
