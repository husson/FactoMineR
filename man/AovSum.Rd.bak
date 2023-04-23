\name{AovSum}

\alias{AovSum}

\title{Analysis of variance with the contrasts sum (the sum of the coefficients is 0)}

\description{
Analysis of variance with the contrasts sum (the sum of the coefficients is 0)
\cr
Test for all the coefficients\cr
Handle missing values
}

\usage{
AovSum(formula, data, na.action = na.omit, ...)
}

\arguments{
  \item{formula}{the formula for the model 'y~x1+x2+x1:x2'}
  \item{data}{a data-frame}
  \item{na.action}{(where relevant) information returned by model.frame on the special handling of NAs.}
  \item{\dots}{other arguments, cf the function \code{lm}}
}

\value{
Retourne des objets
  \item{Ftest}{a table with the F-tests}
  \item{Ttest}{a table with the t-tests}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{aov}}, \code{\link{lm}}}

\examples{
## Example two-way anova
data(senso)
res <- AovSum(Score~ Product + Day , data=senso)
res

## Example two-way anova with interaction
data(senso)
res2 <- AovSum(Score~ Product + Day + Product : Day, data=senso)
res2

## Example ancova
data(footsize)
res3 <- AovSum(footsize ~ size + sex + size : sex, data=footsize)
res3
}

\keyword{models}
