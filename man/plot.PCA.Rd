\name{plot.PCA}

\alias{plot.PCA}

\title{Draw the Principal Component Analysis (PCA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (PCA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.
}

\usage{
\method{plot}{PCA}(x, axes = c(1, 2), choix = c("ind","var","varcor"),
    ellipse = NULL, xlim = NULL, ylim = NULL, habillage="none", 
    col.hab = NULL, col.ind="black", col.ind.sup="blue", 
    col.quali="magenta", col.quanti.sup="blue", col.var="black",
    label = c("all","none","ind","ind.sup","quali","var","quanti.sup"),
	invisible = c("none","ind","ind.sup","quali","var","quanti.sup"), 
    lim.cos2.var = 0., title = NULL, palette=NULL,
    autoLab = c("auto","yes","no"), new.plot = FALSE, select = NULL, 
	unselect = 0.7, shadowtext = FALSE, legend = list(bty = "y", x = "topleft"),
	\dots)
    }

\arguments{
  \item{x}{an object of class PCA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables, "varcor" for a graph with the correlation circle when \code{scale.unit=FALSE})}
  \item{ellipse}{boolean (NULL by default), if not null, draw ellipses around the individuals, and use the results of \code{coord.ellipse}}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{habillage}{give no color for the individuals ("none"), a color for each individual ("ind"),  or color the individuals among a categorical variable (give the number of the categorical variable)}
  \item{col.hab}{a vector with the color to use for the individuals}
  \item{col.ind}{a color for the individuals only if there is not habillage}
  \item{col.ind.sup}{a color for the supplementary individuals only if there is not habillage}
  \item{col.quali}{a color for the categories of categorical variables only  if there is not habillage}
  \item{col.quanti.sup}{a color for the quantitative supplementary variables}
  \item{col.var}{a color for the variables}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("ind", ind.sup", "quali", "var", "quanti.sup"))}
  \item{invisible}{string indicating if some points should not be drawn ("ind", "ind.sup" or "quali" for the individual graph and "var" or "quanti.sup" for
  the correlation circle graph)}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{select}{a selection of the elements that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if \code{unselect=1} the transparceny is total and the elements are not drawn, if \code{unselect=0} the elements are drawn as usual but without any label) or may be a color (for example \code{unselect="grey60"})}
  \item{shadowtext}{boolean; if true put a shadow on the labels (rectangles are written under the labels which may lead to difficulties to modify the graph with another program)}
  \item{legend}{a list of arguments that defines the legend if needed (when individuals are drawn according to a variable); see the arguments of the function \code{legend}}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\details{
The argument \code{autoLab = "yes"} is time-consuming if there are many labels that overlap. In this case, you can modify the size of the characters in order to have less overlapping, using for example cex=0.7.\cr
The \code{select} argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variables) that are drawn.
For example, you can use:\cr
\code{select = 1:5} and then the elements 1:5 are drawn.\cr 
\code{select = c("name1","name5")} and then the elements that have the names name1 and name5 are drawn.\cr 
\code{select = "coord 10"} and then the 10 elements that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr 
\code{select = "contrib 10"} and then the 10 elements that have the highest contribution on the 2 dimensions of your plot are drawn.\cr 
\code{select = "cos2 5"} and then the 5 elements that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr 
\code{select = "dist 8"} and then the 8 elements that have the highest distance to the center of gravity are drawn.\cr 
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}, Jeremy Mazet}

\seealso{ \code{\link{PCA}}}

\examples{
data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
plot(res.pca, habillage = 13, col.hab=c("green","blue"))
## To automatically draw ellipses around the barycentres of all the categorical variables
plotellipses(res.pca)
## Selection of some individuals
plot(res.pca,select="contrib 7") #plot the 7 individuals that have the highest cos2 
plot(res.pca,select="cos2 0.8")  #plot the individuals that have a cos2 greater than 0.8
plot(res.pca,select="cos2 5")     #plot the 5 individuals that have the highest cos2 
plot(res.pca,choix="var",select="cos2 0.6")  #plot the variables that have a cos2 greater than 0.6
}

\keyword{dplot}
