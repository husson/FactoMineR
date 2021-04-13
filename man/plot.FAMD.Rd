\name{plot.FAMD}

\alias{plot.FAMD}

\title{Draw the Multiple Factor Analysis for Mixt Data graphs}

\description{
It provides the graphical outputs associated with the principal component method for mixed data: FAMD.
}

\usage{
\method{plot}{FAMD}(x, choix = c("ind","var","quanti","quali"), axes = c(1, 2), 
    lab.var = TRUE, lab.ind = TRUE, habillage = "none", col.lab = FALSE,
    col.hab = NULL, invisible = NULL, lim.cos2.var = 0., xlim = NULL,
    ylim = NULL, title = NULL, palette=NULL, autoLab = c("auto","yes","no"), 
	new.plot = FALSE, select = NULL, unselect = 0.7, shadowtext = FALSE, 
	legend = list(bty = "y", x = "topleft"),
	graph.type = c("ggplot","classic"), ggoptions = NULL, \dots)
}

\arguments{
  \item{x}{an object of class FAMD} 
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or categorical
  variables graph, "var" for all the variables (quantitative and categorical), "quanti" for the correlation circle)}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{lab.var}{boolean indicating if the labelled of the variables should be drawn on the map}
  \item{lab.ind}{boolean indicating if the labelled of the individuals should be drawn on the map}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual else if it is the name 
  or the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{col.lab}{boolean indicating if the labelled should be colored}
  \item{col.hab}{vector indicating the colors to use to labelled the rows or columns elements chosen in habillage}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or supplementary individuals (invisible="ind.sup") or the centerg of gravity of the categorical variables 
  (invisible= "quali"); if invisible = c("ind","ind.sup"), just the centers of gravity are drawn}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{select}{a selection of the elements that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if \code{unselect=1} the transparceny is total and the elements are not drawn, if \code{unselect=0} the elements are drawn as usual but without any label) or may be a color (for example \code{unselect="grey60"})}
  \item{shadowtext}{boolean; if true put a shadow on the labels (rectangles are written under the labels which may lead to difficulties to modify the graph with another program)}
  \item{legend}{a list of arguments that defines the legend if needed (when individuals are drawn according to a variable); see the arguments of the function \code{legend}}
  \item{graph.type}{a character that gives the type of graph used: "ggplot" or "classic"}
  \item{ggoptions}{a list that gives the graph options when grah.type="ggplot" is used. See the optines and the default values in the details section}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\value{
Returns the individuals factor map and the variables factor map.
}

\examples{
\dontrun{
data(geomorphology)
res <- FAMD(geomorphology)
plot(res,choix="ind",habillage=4)
}
}
\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{FAMD}}}

\keyword{dplot}
