\name{graph.var}

\alias{graph.var}

\title{Make graph of variables}

\description{
Plot the graphs of the variables after  a Factor Analysis.
}

\usage{
graph.var(x, axes = c(1, 2), 
    xlim = NULL, ylim = NULL, col.sup = "blue", 
    col.var = "black", draw="all", label=draw, lim.cos2.var = 0.1,
    cex = 1, title = NULL, new.plot = TRUE, \dots)
    }

\arguments{
  \item{x}{an object of class PCA, MCA, MFA or HMFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{col.sup}{a color for the quantitative supplementary variables}
  \item{col.var}{a color for the variables}
  \item{draw}{a list of character for the variables which are drawn (by default, all the variables are drawn). You can draw all the active variables
  by putting "var" and/or all the supplementary variables by putting "quanti.sup" and/or a list with the names of the variables which should be drawn}
  \item{label}{a list of character for the variables which are labelled (by default, all the drawn variables are labelled). You can label all the active variables
  by putting "var" and/or all the supplementary variables by putting "quanti.sup" and/or a list with the names of the variables which should be labelled}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the variables factor map.
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{PCA}}, \code{\link{MFA}}, \code{\link{MCA}}, \code{\link{DMFA}}, \code{\link{HMFA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
graph.var (res.pca, draw = c("var","Points"), 
    label = c("Long.jump", "Points"))
}

\keyword{dplot}
