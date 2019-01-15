\name{HMFA}
\alias{HMFA}
\title{Hierarchical Multiple Factor Analysis}
\description{
Performs a hierarchical multiple factor analysis, using an object of class \code{list} of \code{data.frame}.
}
\usage{
HMFA(X,H,type = rep("s", length(H[[1]])), ncp = 5, graph = TRUE,
    axes = c(1,2), name.group = NULL)
}
\arguments{
  \item{X}{a \code{data.frame}}
  \item{H}{a list with one vector for each hierarchical level; in each vector the number of variables or the number of group
  constituting the group}
  \item{type}{the type of variables in each group in the first partition; three possibilities: "c" or "s" for quantitative variables
  (the difference is that for "s", the variables are scaled in the program), "n" for categorical variables; by default, all the variables
  are quantitative and the variables are scaled unit}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{name.group}{a list of vector containing the name of the groups for each level of the hierarchy (by default, NULL and the 
  group are named L1.G1, L1.G2 and so on)}
}

\value{
  Returns a list including:
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{group}{a list with first a list of matrices with the coordinates of the groups for each level and second a matrix with the canonical correlation (correlation between the coordinates of the individuals and the partial points))}
  \item{ind}{a list of matrices with all the results for the active individuals (coordinates, square cosine, contributions)}
  \item{quanti.var}{a list of matrices with all the results for the quantitative variables (coordinates, correlation between variables and axes)}
  \item{quali.var}{a list of matrices with all the results for the supplementary categorical variables (coordinates of each categories of each variables, and v.test which is a criterion with a Normal distribution)}
  \item{partial}{a list of arrays with the coordinates of the partial points for each partition}
}
\references{Le Dien, S. & Pages, J. (2003) Hierarchical Multiple factor analysis: application to the comparison of sensory profiles, \emph{Food Quality and Preferences}, \bold{18 (6)}, 453-464.
}

\author{Sebastien Le, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}
\seealso{ \code{\link{print.HMFA}}, \code{\link{plot.HMFA}}, \code{\link{dimdesc}}}

\examples{ 
data(wine)
hierar <- list(c(2,5,3,10,9,2), c(4,2))
res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)))
}

\keyword{multivariate}
