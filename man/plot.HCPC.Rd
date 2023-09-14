\name{plot.HCPC}

\alias{plot.HCPC}

\title{Plots for Hierarchical Classification on Principle Components (HCPC) results}

\description{
  Plots graphs from a HCPC result: tree, barplot of inertia gains and
  first factor map with or without the tree, in 2 or 3 dimensions.}

\usage{
\method{plot}{HCPC}(x, axes=c(1,2), choice="3D.map", rect=TRUE, 
  draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
  new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
  centers.plot=FALSE, \dots)
}

\arguments{
  \item{x}{A HCPC object, see \code{\link{HCPC}} for details.}
  \item{axes}{a two integers vector.Defines the axes of the factor map
  to plot.}  
 \item{choice}{A string. "tree" plots the tree. "bar" plots bars of
  inertia gains. "map" plots a factor map, individuals colored
  by cluster. "3D.map" plots the same factor map, individuals colored by cluster,
  the tree above.}
  \item{rect}{a boolean. If TRUE, rectangles are drawn around clusters
  if choice ="tree".}
  \item{tree.barplot}{a boolean. If TRUE, the barplot of intra inertia
  losses is added on the tree graph.}
  \item{draw.tree}{A boolean. If TRUE, the tree is projected on the
  factor map if choice ="map".}
  \item{ind.names}{A boolean. If TRUE, the individuals names are added
    on the factor map when choice="3D.map" or choice="map"}
  \item{t.level}{Either a positive integer or a string. A positive
  integer indicates the starting level to plot the tree on the map when
  draw.tree=TRUE. If "all", the whole tree is ploted. If "centers", it
  draws the tree starting t the centers of the clusters.}
  \item{title}{a string. Title of the graph. NULL by default and a title is automatically defined}

  \item{centers.plot}{a boolean. If TRUE, the centers of clusters are
  drawn on the 3D factor maps.}
  \item{new.plot}{a boolean. If TRUE, the plot is done in a new window.}
  \item{max.plot}{The max for the bar plot}
  \item{\dots}{Other arguments from other methods.}
}

\value{
  Returns the chosen plot.
}

\author{Guillaume Le Ray, Quentin Molto, Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{HCPC}}}

\examples{
data(iris)
# Clustering, auto nb of clusters:
res.hcpc=HCPC(iris[1:4], nb.clust=3)
# 3D graph from a different point of view:
plot(res.hcpc, choice="3D.map", angle=60)
}
\keyword{dplot}
