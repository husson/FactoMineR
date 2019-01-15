\name{condes}

\alias{condes}

\title{Continuous variable description}

\description{
Description continuous by quantitative variables and/or by categorical variables
}

\usage{
condes(donnee,num.var,weights=NULL,proba = 0.05)}

\arguments{
  \item{donnee}{a data frame made up of at least one quantitative variable and a set of quantitative variables and/or categorical variables}
  \item{num.var}{the number of the variable to characterized}
  \item{weights}{weights for the individuals; if NULL, all individuals has a weight equals to 1; the sum of the weights can be equal to 1 and then the weights will be multiplied by the number of individuals, the sum can be greater than the number of individuals}
  \item{proba}{the significance threshold considered to characterized the category (by default 0.05)}
}

\value{
Returns a list including:
  \item{quanti}{the description of the \code{num.var} variable by the quantitative variables. The variables are sorted in ascending order  (from the one which characterized the most to the one which significantly characterized with the proba \code{proba})}
  \item{quali}{The categorical variables which characterized the continuous variables are listed in ascending order}
  \item{category}{description of the continuous variable \code{num.var} by each category of all the categorical variables}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{catdes}}}

\examples{
data(decathlon)
condes(decathlon, num.var=3)
}

\keyword{multivariate}
