\name{LinearModel}

\alias{LinearModel}

\title{Linear Model with AIC or BIC selection, and with the contrasts 
     sum (the sum of the coefficients is 0) if any categorical variables}

\description{
Linear Model with AIC or BIC selection, and with the contrasts sum (the sum of the coefficients is 0) if any categorical variables \cr
Test for all the coefficients\cr
Handle missing values
}

\usage{
LinearModel(formula, data, na.action = na.omit, type = c("III","II", 3, 2), 
       selection=c("none","aic","bic"), ...)
}

\arguments{
  \item{formula}{the formula for the model 'y~x1+x2+x1:x2'}
  \item{data}{a data-frame}
  \item{na.action}{(where relevant) information returned by model.frame on the special handling of NAs.}
  \item{type}{type of test, "III", "II", 3 or 2. Roman numerals are equivalent to the 
        corresponding Arabic numerals.}
  \item{selection}{a string that defines the model selection according to
        "BIC" for Bayesian Information Criterion or "AIC" for Akaike Information Criterion; 
		"none", by defaut, means that there is no selection.}
  \item{\dots}{other arguments, cf the function \code{lm}}
}

\details{
The Anova function of the package car is used to calculate the F-tests.

The t-tests are obtained using the contrasts "contr.sum" which means that 'sum to zero contrasts'.

A stepwise procedure (using both backword and forward selections) is performed to select a 
 model if selection="AIC" or selection="BIC".}

\value{
The outouts
  \item{Ftest}{a table with the F-tests}
  \item{Ttest}{a table with the t-tests}
  \item{lmResult}{the summary of the function lm}
  \item{call}{the matched call}
  \item{lmResultComp}{the summary of the lm function for the complete model (given only if a selection is performed)}
  \item{callComp}{the matched call for the complete model (given only if a selection is performed)}
}


\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{aov}}, \code{\link{lm}}, \code{\link{print.LinearModel}}}

\examples{
## Example two-way anova
data(senso)
res <- LinearModel(Score~ Product + Day , data=senso, selection="none")
res
## Perform means comparison
meansComp(res,~Product)

## Example two-way anova with interaction
data(senso)
res2 <- LinearModel(Score~ Product + Day + Product : Day, data=senso, selection="none")
res2
meansComp(res,~Product:Day)

## Example two-way anova with selection
data(senso)
res2 <- LinearModel(Score~ Product + Day + Product : Day, data=senso, selection="BIC")
res2

## Example ancova
data(footsize)
res3 <- LinearModel(footsize ~ size + sex + size : sex, data=footsize)
res3
}

\keyword{models}
