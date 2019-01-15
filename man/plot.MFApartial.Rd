\name{plotMFApartial}

\alias{plotMFApartial}

\title{Plot an interactive Multiple Factor Analysis (MFA) graph}

\description{
Draw an interactive Multiple Factor Analysis (MFA) graphs.
}

\usage{
plotMFApartial(x, axes = c(1, 2), 
    lab.ind = TRUE, lab.par = FALSE, habillage = "ind",
    chrono = FALSE, col.hab = NULL, invisible = NULL, 
    draw.partial = NULL, xlim = NULL, ylim = NULL, 
    cex = 1, title = NULL, palette = NULL, \dots)
}

\arguments{ 
  \item{x}{an object of class MFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.ind}{boolean, if TRUE, the label of the mean points are drawn}
  \item{lab.par}{boolean, if TRUE, the label of the partial points are drawn}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual;
  if "quali" the individuals are colored according to one categorical variable; if "group" the individuals are colored according to the group}
  \item{chrono}{boolean, if TRUE, the partial points of a same point are linked (useful when groups correspond to different moment)}
  \item{col.hab}{the colors to use. By default, colors are chosen}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or supplementary individuals (invisible="ind.sup") or the centerg of gravity of the categorical variables 
  (invisible= "quali"); if invisible = c("ind","ind.sup"), just the centers of gravity are drawn}
  \item{draw.partial}{data frame of a boolean variable for all the individuals and all the centers of gravity and with for which the partial points should be drawn (by
  default, NULL and no partial points are drawn)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Draw a graph with the individuals and the centers of gravity. The graph is interactive and clicking on a point will draw the partial points,
if you click on a point for which the partial points are yet drawn, the partial points are deleted.
To stop the interactive plot, click on the title (or in the top of the graph)
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}, Jeremy Mazet}

\seealso{ \code{\link{MFA}}, \code{\link{plot.MFA}}}

\examples{
\dontrun{
data(wine)
res.wine = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
    name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6),graph=FALSE)
liste = plotMFApartial(res.wine)
}
}

\keyword{dplot}
