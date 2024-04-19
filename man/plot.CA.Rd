\name{plot.CA}

\alias{plot.CA}

\title{Draw the Correspondence Analysis (CA) graphs}

\description{
Draw the Correspondence Analysis (CA) graphs.
}

\usage{
\method{plot}{CA}(x, axes = c(1, 2),
    xlim = NULL, ylim = NULL, 
	invisible = c("none","row","col","row.sup","col.sup","quali.sup"), 
	choix = c("CA","quanti.sup"), col.row="blue", col.col="red", 
	col.row.sup="darkblue", col.col.sup="darkred", 
    col.quali.sup="magenta", col.quanti.sup="blue",
	label = c("all","none","row","row.sup","col","col.sup","quali.sup","quanti.sup"), 
    title = NULL, palette = NULL, autoLab = c("auto","yes","no"), 
	new.plot=FALSE, selectRow = NULL, selectCol = NULL,
	unselect = 0.7, shadowtext = FALSE, habillage = "none",
	legend = list(bty = "y", x = "topleft"), 
	graph.type = c("ggplot","classic"), ggoptions = NULL, \dots)
}

\arguments{
  \item{x}{an object of class CA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should be unlabelled ("row", "col", "row.sup", "col.sup","quali.sup")}
  \item{choix}{the graph to plot ("CA" for the CA map, "quanti.sup" for the supplementary quantitative variables)}
  \item{col.row}{a color for the rows points}
  \item{col.col}{a color for columns points}
  \item{col.row.sup}{a color for the supplementary rows points}
  \item{col.col.sup}{a color for supplementary columns points}
  \item{col.quali.sup}{a color for the supplementary categorical variables}
  \item{col.quanti.sup}{a color for the supplementary quantitative variables}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("row", "row.sup", "col", "col.sup","quali.sup","quanti.sup")}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{selectRow}{a selection of the rows that are drawn; see the details section}
  \item{selectCol}{a selection of the columns that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if \code{unselect=1} the transparceny is total and the elements are not drawn, if \code{unselect=0} the elements are drawn as usual but without any label) or may be a color (for example \code{unselect="grey60"})}
  \item{shadowtext}{boolean; if true put a shadow on the labels (rectangles are written under the labels which may lead to difficulties to modify the graph with another program)}
  \item{habillage}{color the individuals among a categorical variable (give the number of the categorical supplementary variable or its name)}
  \item{legend}{a list of arguments that defines the legend if needed (when individuals are drawn according to a variable); see the arguments of the function \code{legend}}
  \item{graph.type}{a character that gives the type of graph used: "ggplot" or "classic"}
  \item{ggoptions}{a list that gives the graph options when grah.type="ggplot" is used. See the optines and the default values in the details section}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\value{
Returns the factor map with the joint plot of CA.
}

\details{
The argument \code{autoLab = "yes"} is time-consuming if there are many labels that overlap. In this case, you can modify the size of the characters in order to have less overlapping, using for example cex=0.7.\cr
The \code{selectRow} and \code{selectCol} arguments can be used in order to select a part of the elements that are drawn.
For example, you can use:\cr
\code{selectRow = 1:5} and then the rows 1 to 5 are drawn.\cr 
\code{select = c("name1","name5")} and then the rows that have the names name1 and name5 are drawn.\cr 
\code{select = "coord 10"} and then the 10 rows (10 active and 10 supplementaries) that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr 
\code{select = "contrib 10"} and then the 10 rows (10 active) that have the highest contribution on the 2 dimensions of your plot are drawn.\cr 
\code{select = "cos2 5"} and then the 5 rows (5 actives and 5 supplementaries) that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr 
\code{select = "dist 8"} and then the 8 rows (8 actives and 8 supplementaries) that have the highest distance to the center of gravity are drawn.\cr 
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{CA}}}

\examples{
data(children)
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)

## select rows and columns that have a cos2 greater than 0.8
plot(res.ca, selectCol="cos2 0.8", selectRow="cos2 0.8")

\dontrun{
## You can modify the ggplot graphs as ususal with ggplot2
require(ggplot2)
gr <- plot(res.ca)
gr + theme(panel.grid.major = element_blank(),
   plot.title=element_text(size=14, color="blue"),
   axis.title = element_text(size=12, color="red"))
}
}

\keyword{dplot}
