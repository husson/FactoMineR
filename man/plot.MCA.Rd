\name{plot.MCA}

\alias{plot.MCA}

\title{Draw the Multiple Correspondence Analysis (MCA) graphs}

\description{
Draw the Multiple Correspondence Analysis (MCA) graphs.
}

\usage{
\method{plot}{MCA}(x, axes = c(1, 2), choix=c("ind","var","quanti.sup"),
    xlim = NULL, ylim = NULL, 
    invisible = c("none","ind","var","ind.sup","quali.sup","quanti.sup"),
    col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
    col.ind.sup = "darkblue", col.quanti.sup = "blue",
    label = c("all","none","ind","var","ind.sup","quali.sup","quanti.sup"),
    title = NULL, habillage = "none", 
    palette = NULL, autoLab = c("auto","yes","no"), new.plot = FALSE, 
    select = NULL, selectMod = NULL, unselect = 0.7, 
	shadowtext = FALSE, legend = list(bty = "y", x = "topleft"), 
	graph.type = c("ggplot","classic"), ggoptions = NULL, \dots)
}

\arguments{
  \item{x}{an object of class MCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals and the categories, "var" for the variables, "quanti.sup" for the supplementary quantitative variables)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{invisible}{string indicating if some points should not be drawn ("ind", "var", "ind.sup", "quali.sup", "quanti.sup")}
  \item{col.ind}{a color for the individuals, if color ="none" the label is not written}
  \item{col.var}{a color for the categories of categorical variables, if color ="none" the label is not written}
  \item{col.quali.sup}{a color for the categorical supplementary variables, if color ="none" the label is not written}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage, if color ="none" the label is not written}
  \item{col.quanti.sup}{a color for the supplementary quantitative variables, if color ="none" the label is not written}
  \item{label}{print the labels of the points; "all" print all the labels; may be a vector with "ind" (for the individuals),"ind.sup" (for the supplementary individuals),"var" (for the active categories), "quali.sup" "var" (for the supplementary categories)}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{habillage}{string corresponding to the color which are used. If "none", one color is used for the individual, another one for the categorical variables;
  if "quali", one color is used for each categorical variables; else if it is the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{select}{a selection of the elements that are drawn; see the details section}
  \item{selectMod}{a selection of the categories that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if \code{unselect=1} the transparceny is total and the elements are not drawn, if \code{unselect=0} the elements are drawn as usual but without any label) or may be a color (for example \code{unselect="grey60"})}
  \item{shadowtext}{boolean; if true put a shadow on the labels (rectangles are written under the labels which may lead to difficulties to modify the graph with another program)}
  \item{legend}{a list of arguments that defines the legend if needed (when individuals are drawn according to a variable); see the arguments of the function \code{legend}}
  \item{graph.type}{a character that gives the type of graph used: "ggplot" or "classic"}
  \item{ggoptions}{a list that gives the graph options when grah.type="ggplot" is used. See the optines and the default values in the details section}
\item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\details{
The argument \code{autoLab = "yes"} is time-consuming if there are many labels that overlap. In this case, you can modify the size of the characters in order to have less overlapping, using for example cex=0.7.\cr
The \code{select} argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variabless) that are drawn.
For example, you can use:\cr
\code{select = 1:5} and then the elements 1:5 are drawn.\cr 
\code{select = c("name1","name5")} and then the elements that have the names name1 and name5 are drawn.\cr 
\code{select = "coord 10"} and then the 10 elements that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr 
\code{select = "contrib 10"} and then the 10 elements that have the highest contribution on the 2 dimensions of your plot are drawn.\cr 
\code{select = "cos2 5"} and then the 5 elements that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr 
\code{select = "dist 8"} and then the 8 elements that have the highest distance to the center of gravity are drawn.\cr 
The \code{selectMod} argument can be used in order to select the categories that are drawn.
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MCA}}}

\examples{
data (poison)
res.mca = MCA (poison, quali.sup = 3:4, quanti.sup = 1:2, graph=FALSE)
plot(res.mca,invisible=c("var","quali.sup"))
plot(res.mca,invisible="ind")
plot(res.mca,choix="var")
plot(res.mca,invisible=c("ind"),autoLab="yes", selectMod="cos2 10")
\dontrun{
plot(res.mca,autoLab="yes", selectMod="cos2 5", select="cos2 5")
}
}

\keyword{dplot}
