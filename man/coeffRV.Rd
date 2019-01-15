\name{coeffRV}

\alias{coeffRV}

\title{Calculate the RV coefficient and test its significance}

\description{
Calculate the RV coefficient and test its significance.
}
\usage{
coeffRV(X, Y)
}

\arguments{
  \item{X}{a matrix with \emph{n} rows (individuals) and \emph{p} numerous columns (variables)}
  \item{Y}{a matrix with \emph{n} rows (individuals) and \emph{p} numerous columns (variables)}
}

\details{
Calculates the RV coefficient between \code{X} and \code{Y}. It returns also the standardized RV, 
the expectation, the variance and the skewness under the permutation
distribution. These moments are used to approximate the exact
distribution of the RV statistic with the Pearson type III approximation and the p-value associated to this test is given.
}


\value{
A list containing the following components:
\item{RV}{the RV coefficient between the two matrices}
\item{RVs}{the standardized RV coefficients}
\item{mean}{the mean of the RV permutation distribution}
\item{variance}{the variance of the RV permutation distribution}
\item{skewness}{the skewness of the RV permutation distribution}
\item{p.value}{the p-value associated to the test of the significativity of the RV coefficient (with the Pearson type III approximation}
}

\references{
Escouffier, Y. (1973) \emph{Le traitement des variables vectorielles}. Biometrics \bold{29} 751--760.\cr
Josse, J., Husson, F., Pag\`es, J. (2007) \emph{Testing the significance of the RV coefficient}. 
  Computational Statististics and Data Analysis. \bold{53} 82--91.\cr
Kazi-Aoual, F., Hitier, S., Sabatier, R., Lebreton, J.-D., (1995) Refined approximations to permutations tests 
for multivariate inference. Computational Statistics and Data Analysis, \bold{20}, 643--656 \cr
}

\author{Julie Josse, Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\examples{
data(wine)
X <- wine[,3:7]
Y <- wine[,11:20]
coeffRV(X,Y)
}  
\keyword{multivariate}
