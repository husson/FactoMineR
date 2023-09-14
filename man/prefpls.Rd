\name{prefpls}

\alias{prefpls}

\title{Scatter plot and additional variables with quality of representation contour lines}

\description{
This function is useful to interpret the usual graphs $(x,y)$ with additional
quantitative variables.
}

\usage{
prefpls(donnee, var1 = 1, var2 = 2, firstvar = 3, 
    lastvar = ncol(donnee), levels = c(0.2,0.4,0.6,0.7,0.8,0.9,1), 
    asp = 1, nbchar = max(nchar(colnames(donnee))), title = NULL,
    choix="var")
}

\arguments{
  \item{donnee}{a data frame made up of quantitative variables}  
  \item{var1}{the position of the variable corresponding to the x-axis}
  \item{var2}{the position of the variable corresponding to the y-axis}
  \item{firstvar}{the position of the first endogenous variable}
  \item{lastvar}{the position of the last endogenous variable (by default the last column of \code{donnee})}
  \item{levels}{a list of the levels displayed in the graph of variables}
  \item{asp}{aspect ratio for the graph of the individuals}
  \item{nbchar}{the number of characters used for the labels of the variables}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{choix}{the graph to plot ("ind" for the individuals, "var" for the variables)}
}

\details{
This function is very useful when there is a strong correlation between two variables \emph{x} and \emph{y}
}

\value{
A scatter plot of the invividuals\cr
A graph with additional variables and the quality of representation contour lines.
}

\references{
Husson, F. & Pages, J. (2005). Scatter plot and additional variables. \emph{Journal of applied statistics}}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\examples{
data(decathlon)
prefpls(decathlon[,c(11,12,1:10)])
}

\keyword{dplot}
