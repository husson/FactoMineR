\name{plot.MFA}

\alias{plot.MFA}

\title{Draw the Multiple Factor Analysis (MFA) graphs}

\description{
Draw the Multiple Factor Analysis (MFA) graphs.
}

\usage{
\method{plot}{MFA}(x, axes = c(1, 2), choix = c("ind","var","group","axes","freq"), 
    ellipse=NULL, ellipse.par=NULL,
    lab.grpe=TRUE, lab.var=TRUE, lab.ind=TRUE, 
    lab.par=FALSE, lab.col=TRUE, ncp=2, habillage="group", col.hab=NULL, 
    invisible = c("none","ind","ind.sup","quanti","quanti.sup",
	"quali","quali.sup","row","row.sup","col","col.sup"), 
	partial = NULL, lim.cos2.var = 0., 
    chrono = FALSE, xlim = NULL, ylim = NULL, 
    title = NULL, palette = NULL, 
	autoLab = c("auto","yes","no"), new.plot = FALSE, 
	select = NULL, unselect = 0.7, shadowtext = FALSE, 
	legend = list(bty = "y", x = "topleft"),
	graph.type = c("ggplot","classic"), ggoptions = NULL, \dots)
}

\arguments{
  \item{x}{an object of class MFA}
  \item{choix}{a string corresponding to the graph that you want to do ("ind" for the individual or categorical
  variables graph, "var" for the quantitative variables graph, "freq" for the frequence or contingency tables,
  "axes" for the graph of the partial axes, "group" for the groups representation)}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{ellipse}{boolean (NULL by default), if not null, draw ellipses around the individuals, and use the results of \code{coord.ellipse}}
  \item{ellipse.par}{boolean (NULL by default), if not null, draw ellipses around the partial individuals, and use the results of \code{coord.ellipse}}
  \item{lab.grpe}{boolean, if TRUE, the labels of the groups are drawn}
  \item{lab.var}{boolean, if TRUE, the labels of the variables are drawn}
  \item{lab.ind}{boolean, if TRUE, the labels of the mean points are drawn}
  \item{lab.par}{boolean, if TRUE, the labels of the partial points are drawn}
  \item{lab.col}{boolean, if TRUE, the labels of the columns for the contingency tables are drawn}
  \item{ncp}{number of principal components drawn for the separate analyses for the graph of the partial axes}
  \item{habillage}{string corresponding to the color which are used. If "ind", one color is used for each individual;
  if "group" the individuals are colored according to the group; else if it is the name or the position of a categorical variable, it colors 
  according to the different categories of this variable}
  \item{col.hab}{the colors to use. By default, colors are chosen}
  \item{invisible}{list of string; for choix ="ind", the individuals can be omit (invisible = "ind"),
  or supplementary individuals (invisible="ind.sup") or the center of gravity of the categorical variables 
  (invisible= "quali" or "quali.sup" for the supplementary categories); 
  if invisible = c("ind","ind.sup"), just the centers of gravity are drawn; if choix="var", invisible="quanti" suppress the
  active variable and invisible = "quanti.sup" suppress the supplementary variables}
  \item{partial}{list of the individuals or of the center of gravity for which the partial points should be drawn (by
  default, partial = NULL and no partial points are drawn)}
  \item{lim.cos2.var}{value of the square cosinus under with the points are not drawn}
  \item{chrono}{boolean, if TRUE, the partial points of a same point are linked (useful when groups correspond to different moment)}
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

\details{
The argument \code{autoLab = "yes"} is time-consuming if there are many labels that overlap. In this case, you can modify the size of the characters in order to have less overlapping, using for example cex=0.7.\cr
The \code{select} argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variabless) that are drawn.
For example, you can use:\cr
\code{select = 1:5} and then the elements 1:5 are drawn.\cr 
\code{select = c("name1","name5")} and then the elements that have the names name1 and name5 are drawn.\cr 
\code{select = "coord 10"} and then the 10 elements (individuals, variables, frequencies) that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr 
\code{select = "contrib 10"} and then the 10 elements (individuals, variables, frequencies) that have the highest contribution on the 2 dimensions of your plot are drawn.\cr 
\code{select = "cos2 5"} and then the 5 elements (individuals, variables, frequencies) that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr 
\code{selectMod = "contrib 5"}, the 5 categories that contribute the most to the two dimensions are drawn.\cr 
\code{selectMod = "cos2 0.5"}, the categories that have a cos2 greater than 0.5 on the two dimensions are drawn.\cr 
\code{selectMod = "v.test 2"}, the categories that have a v.test higher than the value on one of the two dimensions are drawn.\cr 
}

\value{
Returns the individuals factor map and the variables factor map.
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}, Jeremy Mazet}

\seealso{ \code{\link{MFA}}}

\examples{
\dontrun{
data(wine)
res <- MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
    name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6),graph=FALSE)
plot(res, choix = "ind")
plot(res, choix = "ind", partial="all")
plot(res, choix = "ind", habillage="Label")
plot(res, choix = "var", habillage="group")
plot(res, choix = "axes")

data(wine)
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))
summary(res)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))

#### Confidence ellipses around categories per variable
plotellipses(res)
plotellipses(res,keepvar="Label") ## for 1 variable

#### Interactive graph
liste = plotMFApartial(res)
plot(res,choix="ind",habillage = "Terroir")

###Example with groups of categorical variables
data (poison)
MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
    name.group=c("desc","desc2","symptom","eat"),
    num.group.sup=1:2)

###Example with groups of frequency tables
data(mortality)
res<-MFA(mortality,group=c(9,9),type=c("f","f"),
    name.group=c("1979","2006"))
}
}
\keyword{dplot}
