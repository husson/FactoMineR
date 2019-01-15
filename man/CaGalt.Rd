\name{CaGalt}

\alias{CaGalt}

\title{Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt)}

\description{
Correspondence Analysis on Generalised Aggregated Lexical Table (CaGalt) aims at expanding correspondence analysis on an aggregated lexical table to the case of several quantitative and categorical variables with the objective of establishing a typology of the variables and a typology of the frequencies from their mutual relationships. To avoid the instability issued from multicollinearity among the contextual variables and limit the influence of noisy measurements, the contextual variables are substituted by their principal components. Validation tests in the form of confidence ellipses for the frequencies and the variables are also proposed.
}

\usage{
CaGalt(Y, X, type="s", conf.ellip=FALSE, nb.ellip=100, level.ventil=0,
  sx=NULL, graph=TRUE, axes=c(1,2))
}

\arguments{
  \item{Y}{a data frame with n rows (individuals) and p columns (frequencies)}
  \item{X}{a data frame with n rows (individuals) and k columns (quantitative or categorical variables)}
  \item{type}{the type of variables: "c" or "s" for quantitative variables and "n" for categorical variables. The difference is that for "s" variables are scaled to unit variance (by default, variables are scaled to unit variance)}
  \item{conf.ellip}{boolean (FALSE by default), if TRUE, draw confidence ellipses around the frequencies and the variables when "graph" is TRUE}
  \item{nb.ellip}{number of bootstrap samples to compute the confidence ellipses (by default 100)}
  \item{level.ventil}{proportion corresponding to the level under which the category is ventilated; by default, 0 and no ventilation is done. Available only when type is equal to "n"}	
  \item{sx}{number of principal components kept from the principal axes analysis of the contextual variables (by default is NULL and all principal components are kept)}
  \item{graph}{boolean, if TRUE a graph is displayed}
  \item{axes}{a length 2 vector specifying the components to plot}
}

\value{
Returns a list including: 
  \item{eig}{a matrix containing all the eigenvalues, the percentage of variance and the cumulative percentage of variance}
  \item{ind}{a list of matrices containing all the results for the individuals (coordinates, square cosine)}  
  \item{freq}{a list of matrices containing all the results for the frequencies (coordinates, square cosine, contributions)}
  \item{quanti.var}{a list of matrices containing all the results for the quantitative variables (coordinates, correlation between variables and axes, square cosine)}
  \item{quali.var}{a list of matrices containing all the results for the categorical variables (coordinates of each categories of each variables, square cosine)}
  \item{ellip}{a list of matrices containing the coordinates of the frequencies and variables for replicated samples from which the confidence ellipses are constructed} 
  
Returns the individuals, the frequencies and the variables factor map. If there are more than 50 frequencies, the first 50 frequencies that have the highest contribution on the 2 dimensions of your plot are drawn.
The plots may be improved using the argument autolab, modifying the size of the labels or selecting some elements thanks to the plot.CaGalt function. 
}

\references{
Becue-Bertaut, M., Pages, J. and Kostov, B. (2014). Untangling the influence of several contextual variables on
the respondents'\ lexical choices. A statistical approach.\emph{SORT}
Becue-Bertaut, M. and Pages, J. (2014). Correspondence analysis of textual data involving contextual information:
Ca-galt on principal components.\emph{Advances in Data Analysis and Classification}
}

\author{Belchin Kostov \email{badriyan@clinic.ub.es}, Monica Becue-Bertaut, Francois Husson}

\seealso{ \code{\link{print.CaGalt}}, \code{\link{summary.CaGalt}}, \code{\link{plot.CaGalt}}}

\examples{

\dontrun{
###Example with categorical variables
data(health)
res.cagalt<-CaGalt(Y=health[,1:115],X=health[,116:118],type="n")
}
}
\keyword{multivariate}
