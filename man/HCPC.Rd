\name{HCPC}

\alias{HCPC}

\title{Hierarchical Clustering on Principle Components (HCPC)}

\description{
  Performs an agglomerative hierarchical clustering on results from a
  factor analysis. It is possible to cut the tree by clicking at the
  suggested (or an other) level. Results include paragons, description
  of the clusters, graphics.}

\usage{
HCPC(res, nb.clust=0, consol=TRUE, iter.max=10, min=3, 
  max=NULL, metric="euclidean", method="ward", order=TRUE,
  graph.scale="inertia", nb.par=5, graph=TRUE, proba=0.05, 
  cluster.CA="rows",kk=Inf,description=TRUE,\dots)}

\arguments{
  \item{res}{Either the result of a factor analysis or a dataframe.}
  \item{nb.clust}{an integer. If 0, the tree is cut at the level the user clicks
  on. If -1, the tree is automatically cut at the suggested level (see
  details). If a (positive) integer, the tree is cut with nb.cluters clusters.}
  \item{consol}{a boolean. If TRUE, a k-means consolidation is performed (consolidation cannot be performed if kk is used and equals a number).}
  \item{iter.max}{An integer. The maximum number of iterations for the consolidation.}
  \item{min}{an integer. The least possible number of clusters suggested.}
  \item{max}{an integer. The higher possible number of clusters suggested; by default the minimum between 10 and the number of individuals divided by 2.}
  \item{metric}{The metric used to built the tree. See \code{\link{agnes}} for details. Defaults to "euclidean".}
  \item{method}{The method used to built the tree. See \code{\link{agnes}} for details. Defaults to "ward".}
  \item{order}{A boolean. If TRUE, clusters are ordered following their center
    coordinate on the first axis.}
  \item{graph.scale}{A character string. By default "inertia" and the height of the tree corresponds
   to the inertia gain, else "sqrt-inertia" the square root of the inertia gain.}
  \item{nb.par}{An integer. The number of edited paragons.}
  \item{graph}{If TRUE, graphics are displayed. If FALSE, no graph are displayed.}
  \item{proba}{The probability used to select axes and variables in
  catdes (see \code{\link{catdes}} for details.}
  \item{cluster.CA}{A string equals to "rows" or "columns" for the clustering 
  of Correspondence Analysis results.}
  \item{kk}{An integer corresponding to the number of clusters used in a Kmeans 
  preprocessing before the hierarchical clustering;   the top of the hierarchical 
  tree is then constructed from this partition. This is very useful if the number 
  of individuals is high. Note that consolidation cannot be performed if kk is different 
  from Inf and some graphics are not drawn. Inf is used by default and 
  no preprocessing is done, all the graphical outputs are then given.}
  \item{description}{boolean; if TRUE the clusters are characterized by the variables and the dimensions}
  \item{\dots}{Other arguments from other methods.}
}

\details{
  The function first built a hierarchical tree. Then the sum of the
  within-cluster inertia are calculated for each partition. The suggested
  partition is the one with the higher relative loss of inertia
  (i(clusters n+1)/i(cluster n)).
  
  The absolute loss of inertia (i(cluster n)-i(cluster n+1)) is plotted
  with the tree.  
  
  If the ascending clustering is constructed from a data-frame with a lot of 
  rows (individuals), it is possible to first perform a partition with 
  kk clusters and then construct the tree from the (weighted) kk clusters. 
}

\value{
  Returns a list including:
  \item{data.clust}{The original data with a supplementary column called
    clust containing the partition.}
  \item{desc.var}{The description of the classes by the variables.
     See \code{\link{catdes}} for details or \code{\link{descfreq}} if clustering
	 is performed on CA results.}
  \item{desc.axes}{The description of the classes by the factors (axes). 
     See \code{\link{catdes}} for details.}
  \item{call}{A list or parameters and internal objects. \code{call$t} gives the results for the hierarchical tree; 
  \code{call$bw.before.consol} and \code{call$bw.after.consol} give the between inertia before consolidation (i.e. for the clustering obtained
  from the hierarchical tree) and after the consolidation with Kmeans.}
  \item{desc.ind}{The paragons (para) and the more typical individuals
    of each cluster. See details.}
  
  Returns the tree and a barplot of the inertia gains, the individual
  factor map with the tree (3D), the factor map with individuals coloured
  by cluster (2D).
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}, Guillaume Le Ray, Quentin Molto}

\seealso{ \code{\link{plot.HCPC}}, \code{\link{catdes}},\cr
\href{https://www.youtube.com/watch?v=4XrgWmN9erg&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=7}{Video showing how to perform clustering with FactoMineR}}

\examples{
\dontrun{
data(iris)
# Principal Component Analysis:
res.pca <- PCA(iris[,1:4], graph=FALSE)
# Clustering, auto nb of clusters:
hc <- HCPC(res.pca, nb.clust=-1)

### Construct a hierarchical tree from a partition (with 10 clusters)
### (useful when the number of individuals is very important)
hc2 <- HCPC(iris[,1:4], kk=10, nb.clust=-1)

## Graphical interface
require(Factoshiny)
res <- Factoshiny(iris[,1:4])
}
}
\keyword{multivariate}
