\name{plotellipses}

\alias{plotellipses}

\title{Draw confidence ellipses around the categories}

\description{
Draw confidence ellipses around the categories.
}

\usage{
plotellipses(model, keepvar = "all", axes = c(1, 2), means=TRUE, level = 0.95, 
    magnify = 2, cex = 1, pch = 20, pch.means=15, type = c("g","p"), 
    keepnames = TRUE, namescat = NULL, xlim=NULL, ylim=NULL, lwd=1, 
    label="all", autoLab=c("auto","yes","no"), 
	graph.type = c("ggplot","classic"), \dots)
	}
\arguments{
  \item{model}{an object of class MCA or PCA or MFA}
\item{keepvar}{a boolean or numeric vector of
  indexes of variables or a character vector of names of variables. If
  keepvar is "all", "quali" or "quali.sup" variables which are plotted
  are all the categorical variables, only those which are used to
  compute the dimensions (active variables) or only the supplementary
  categorical variables. If keepvar is a numeric vector of indexes or a
  character vector of names of variables, only relevant variables are plotted.}
\item{axes}{a length 2 vector specifying the components to plot}
\item{means}{boolean which indicates if the confidence ellipses are
    for (the coordinates of) the means of the
    categories (the empirical variance is divided  by the number of
    observations) or for (the coordinates of) the observations of the
    categories}
\item{level}{the confidence level for the ellipses}
\item{magnify}{numeric which control how the level names are
  magnified. A value of 2 means that the level names have character
  expansion equal to two times cex}
\item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
\item{pch}{plotting character for coordinates, cf. function \code{\link{par}} in the \pkg{graphics} package}
\item{pch.means}{plotting character for means, cf. function \code{\link{par}} in the \pkg{graphics} package}
\item{type}{cf. function \code{\link[lattice]{xyplot}}}
\item{keepnames}{a boolean or numeric vector of
  indexes of variables or a character vector of names of variables. If keepnames is TRUE, names of levels are taken
  from the (modified) dataset extracted from modele, if FALSE trimming
  names is done. When trimming, names of levels are taken from the
  (modified) dataset extracted from modele, then, the corresponding
  number of characters of names of original variables plus 1 is
  removed. If keepnames is a vector of indexes or names, trimming is done on
  all variables excepted whose in keepnames}
\item{namescat}{a vector giving for each observation the value of
  categorical variable, each variable are stacked under each other. If
  NULL, names are taken from the (modified) dataset extracted from
  modele} 
\item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
\item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
\item{lwd}{The line width, a positive number, defaulting to 1}
\item{label}{a list of character for the elements which are labelled (by default, "all", you can use "none", "ind", ind.sup"))}
\item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "y" if there are less than 50 elements and "no" otherwise; if "y", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{graph.type}{a character that gives the type of graph used: "ggplot" or "classic"}
\item{\dots}{further arguments passed to or from other methods}
}

\value{
Return a graph with the ellipses.
If only one variable is chosen, the graph is different.
}

\author{Pierre-Andre Cornillon, Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{MCA}}, \code{\link{PCA}}}

\examples{
\dontrun{
data(poison)
res.mca = MCA(poison, quali.sup = 3:4, quanti.sup = 1:2)
plotellipses(res.mca)
plotellipses(res.mca,keepvar=3:6)
}

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
plotellipses(res.pca,keepvar=13)
}

\keyword{multivariate}
