\name{predict.LinearModel}

\alias{predict.LinearModel}

\title{Predict method for Linear Model Fits}

\description{
Predicted values based on LinearModel object.
}

\usage{
\method{predict}{LinearModel}(object, newdata, interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"), \dots)
}

\arguments{
  \item{object}{Object of class inheriting from "LinearModel"}
  \item{newdata}{An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.}
  \item{interval}{Type of interval calculation. Can be abbreviated.}
  \item{level}{Tolerance/confidence level.}
  \item{type}{Type of prediction (response or model term). Can be abbreviated.}
  \item{\dots}{further arguments passed to or from other methods such as \code{lm}.}
}

\details{
See the help of \code{\link{predict.lm}} function.
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{LinearModel}}}

