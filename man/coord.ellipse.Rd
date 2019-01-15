\name{coord.ellipse}

\alias{coord.ellipse}

\title{Construct confidence ellipses}

\description{Construct confidence ellipses
}

\usage{coord.ellipse (coord.simul, centre = NULL, axes = c(1, 2), 
    level.conf = 0.95, npoint = 100, bary = FALSE)
}

\arguments{
  \item{coord.simul}{a data frame containing the coordinates of the individuals
  for which the confidence ellipses are constructed. This data frame can contain more than 2 variables;
  the variables taken into account are chosen after. The first column must be a factor which allows
  to associate one row to an ellipse. The simule object of the result of the simule function
  correspond to a data frame.}
  \item{centre}{a data frame whose columns are the same than those of the coord.simul, and with the coordinates
  of the centre of each ellipse. This parameter is optional and NULL by default; in this case,
  the centre of the ellipses is calculated from the data}
  \item{axes}{a length 2 vector specifying the components of coord.simul that are taken into account}
  \item{level.conf}{confidence level used to construct the ellipses. By default, 0.95}
  \item{npoint}{number of points used to draw the ellipses}
  \item{bary}{boolean, if bary = TRUE, the coordinates of the ellipse around the barycentre of individuals are calculated}
}

\value{
\item{res}{a data frame with (npoint times the number of ellipses) rows and three columns. The first column
is the factor of coord.simul, the two others columns  give the coordinates of the ellipses on the two dimensions chosen.}
\item{call}{the parameters of the function chosen}
}

\author{Jeremy Mazet}

\seealso{ \code{\link{simule}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13,graph=FALSE)
aa <- cbind.data.frame(decathlon[,13],res.pca$ind$coord)
bb <- coord.ellipse(aa,bary=TRUE)
plot(res.pca,habillage=13,ellipse=bb)

## To automatically draw ellipses around the barycentres of all the categorical variables
plotellipses(res.pca)
}

\keyword{dplot}
