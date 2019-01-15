\name{plot.DMFA}

\alias{plot.DMFA}

\title{Draw the Dual Multiple Factor Analysis (DMFA) graphs}

\description{
Plot the graphs for a Principal Component Analysis (DMFA) with supplementary individuals, supplementary quantitative 
variables and supplementary categorical variables.
}

\usage{
\method{plot}{DMFA}(x, axes = c(1, 2), choix = "ind", label="all",
    lim.cos2.var = 0., xlim=NULL, ylim=NULL, title = NULL,
    palette = NULL, new.plot = FALSE, 
	autoLab = c("auto","yes","no"), \dots)
}

\arguments{
  \item{x}{an object of class DMFA}
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
  \item{label}{a list of character for the elements which are labelled (by default, all the elements are labelled ("ind", ind.sup", "quali", "var", "quanti.sup"))}
  \item{lim.cos2.var}{value of the square cosinus under the variables are not drawn}
  \item{xlim}{range for the plotted 'x' values, defaulting to the range of the finite values of 'x'}
  \item{ylim}{range for the plotted 'y' values, defaulting to the range of the finite values of 'y'}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{palette}{the color palette used to draw the points. By default colors are chosen. If you want to define the colors : palette=palette(c("black","red","blue")); or you can use: palette=palette(rainbow(30)), or in black and white for example: palette=palette(gray(seq(0,.9,len=25)))}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
  \item{autoLab}{if \code{autoLab="auto"}, \code{autoLab} is equal to "yes" if there are less than 50 elements and "no" otherwise; if "yes", the labels of the drawn elements are placed in a "good" way (can be time-consuming if many elements), and if "no" the elements are placed quickly but may overlap}
  \item{\dots}{further arguments passed to or from other methods}
}

\value{
Returns the individuals factor map and the variables factor map, the partial variables representation and the groups factor map.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{DMFA}}}

\keyword{dplot}
