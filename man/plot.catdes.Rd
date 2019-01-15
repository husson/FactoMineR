\name{plot.catdes}

\alias{plot.catdes}

\title{Plots for description of clusters (catdes)}

\description{
  Plots a graph from a catdes output.}

\usage{
\method{plot}{catdes}(x, show="all", level=0.01, sort=NULL,
   col.upper="indianred2", col.lower="royalblue1", numchar = 10,
   barplot = FALSE, \dots)
}

\arguments{
  \item{x}{A catdes object, see \code{\link{catdes}} for details.}
  \item{show}{a strig. If "quali", only the categorical variables are
  used. If "quanti", only the the quantitative variables are used. If
  "all", both quali and quanti are used.}
\item{level}{a positive float. Indicates a critical value the p-value.}
\item{sort}{NULL (default) or an integer between 1 and the number of clusters or a character (the name of a group). If it is an integer or the name of a group, the features are sorted according to their significances in the construction of the given cluster.}
\item{col.upper}{The color used for under-represented features.}
\item{col.lower}{The color used for over-represented features.}
  \item{numchar}{number of characters for the labels}
  \item{barplot}{a boolean; if true a barplot per category is drawn, else a table}
  \item{\dots}{further arguments passed to or from other methods}

  }
\value{
if barplot is true, a barplot is drawn per category with variables that significantly describe the category.\cr
If barplot is false; it returns a grid. The rows stand for the clusters and the columns for the significant variables. 
A cell colored in col.lower (resp. col.upper) i.e. by default in blue (resp. red) for a quantitative variable means that the average value of the variable in the given cluster is significantly lower (resp. higher) than in the overall data. 
A cell colored in col.lower (resp. col.upper) for a categorical variable means that the given value of the variable is significantly under-represented (resp. over-represented) in the given cluster than in the overall data. 
The degree of transparency of the color also indicates the significance of the difference between the behavior of the variable in the given cluster and in the overall data. Indeed, the more transparent the cell is, the less significant the difference is. 

}

\author{Guillaume Le Ray, Camille Chanial, Elise Dumas, Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{catdes}}}

\examples{
\dontrun{
data(wine)
res.c=catdes(wine, num.var=2)
plot(res.c)
}
}

\keyword{dplot}
