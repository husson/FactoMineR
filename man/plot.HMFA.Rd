\name{plot.HMFA}

\alias{plot.HMFA}

\title{Draw the Hierarchical Multiple Factor Analysis (HMFA) graphs}

\description{Draw the Hierarchical Multiple Factor Analysis (HMFA) graphs}

\usage{
\method{plot}{HMFA}(x, axes = c(1,2),num=6, choix = "ind", 
    lab.grpe = TRUE, lab.var = TRUE, lab.ind.moy = TRUE, 
    invisible = NULL, lim.cos2.var = 0., 
    xlim = NULL, ylim = NULL, cex = 1, title = NULL, new.plot = FALSE, \dots)
}

\arguments{
  \item{x}{an object of class HMFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{num}{number of grpahs in a same windows}
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or categorical
  variables graph, "var" for the quantitative variables graph, "axes" for the graph of the partial axes,
  "group" for the groups representation)}
  \item{lab.grpe}{boolean, if TRUE, the label of the groups are drawn}
  \item{lab.var}{boolean, if TRUE, the label of the variables are drawn}
  \item{lab.ind.moy}{boolean, if TRUE, the label of the mean points are drawn}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or the centers of gravity of the categorical variables 
  (invisible= "quali")}
  \item{lim.cos2.var}{value of the square cosinus under with the points are not drawn}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Jeremy Mazet, Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{HMFA}}}

\examples{
data(wine)
hierar <- list(c(2,5,3,10,9,2), c(4,2))
res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)), graph = FALSE)
plot(res.hmfa, invisible="quali")
plot(res.hmfa, invisible="ind")
}

\keyword{dplot}
