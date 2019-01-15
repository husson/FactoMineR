\name{plot.CaGalt}

\alias{plot.CaGalt}

\title{Draw the Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) graphs}

\description{
Plot the graphs for a Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt). 
}

\usage{
\method{plot}{CaGalt}(x, axes = c(1, 2), choix = c("ind", "freq", "quali.var", "quanti.var"), 
  conf.ellip = FALSE, contr.ellipse = 3, xlim = NULL, ylim = NULL, col.ind = "black", 
  col.freq = "darkred", col.quali = "blue", col.quanti = "blue", label = TRUE, 
  lim.cos2.var = 0, title = NULL, palette = NULL, 
  autoLab = c("auto", "yes", "no"), new.plot = FALSE, select = NULL, 
  unselect = 0.7, shadowtext = FALSE, \ldots)
}

\arguments{
  \item{x}{an object of class CaGalt}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "freq" for the frequencies, "quali.var" for the categorical variables, "quanti.var" for the quantitative variables)}
  \item{conf.ellip}{boolean (FALSE by default), if TRUE, draw ellipses around the frequencies and the variables}
  \item{contr.ellipse}{the confidence ellipses were drawn for the frequencies with a contribution higher than X times of mean contribution on the 2 dimensions of your plot (by default 3)}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{col.ind}{a color for the individuals (by default "black")}
  \item{col.freq}{a color for the frequencies (by default "darkred")}
  \item{col.quali}{a color for the categories of categorical variables (by default "blue")}
  \item{col.quanti}{a color for the quantitative variables (by default "blue")}
  \item{label}{the labels are drawn (by default TRUE)}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{autoLab}{if autoLab="auto", autoLab is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{select}{a selection of the elements that are drawn; see the details section}
  \item{unselect}{may be either a value between 0 and 1 that gives the transparency of the unselected objects (if unselect=1 the transparency is total and the elements are not drawn, if unselect=0 the elements are drawn as usual but without any label) or may be a color (for example unselect="grey60")}
  \item{shadowtext}{boolean; if true put a shadow on the labels (rectangles are written under the labels which may lead to difficulties to modify the graph with another program)}
  \item{...}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\details{
The argument autoLab = "yes" is time-consuming if there are many labels that overlap. In this case, you can modify the size of the characters in order to have less overlapping, using for example cex=0.7.
The select argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variabless) that are drawn. For example, you can use:
select = 1:5 and then the elements 1:5 are drawn.
select = c("name1","name5") and then the elements that have the names name1 and name5 are drawn.
select = "coord 10" and then the 10 elements that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.
select = "contrib 10" and then the 10 elements that have the highest contribution on the 2 dimensions of your plot are drawn (available only when frequencies are drawn).
select = "cos2 5" and then the 5 elements that have the highest cos2 on the 2 dimensions of your plot are drawn.
}

\value{
Returns the individuals, the frequencies and the variables factor map.
}

\author{Belchin Kostov \email{badriyan@clinic.ub.es}, Monica Becue-Bertaut, Francois Husson}

\seealso{ \code{\link{CaGalt}}}

\examples{

\dontrun{
data(health)
res.cagalt<-CaGalt(Y=health[,1:115],X=health[,116:118],type="n")
plot(res.cagalt,choix="quali.var",conf.ellip=TRUE,axes=c(1,4))

## Selection of some individuals,categories and frequencies
plot(res.cagalt,choix="freq",col.freq="darkgreen",cex=1.5,select="contrib 10") 
plot(res.cagalt,choix="ind",select="coord 10") 
plot(res.cagalt,choix="quali.var",select="cos2 0.5") 
}
}
\keyword{multivariate}
