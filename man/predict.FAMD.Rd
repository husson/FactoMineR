\name{predict.FAMD}

\alias{predict.FAMD}

\title{Predict projection for new rows with Factor Analysis of Mixed Data}

\description{
Predict the projection of new rows with Factor Analysis of Mixed Data.
}

\usage{
\method{predict}{FAMD}(object, newdata, \dots)
}

\arguments{
  \item{object}{an object of class FAMD}
  \item{newdata}{A data frame or a matrix in which to look for variables with which to predict. newdata must contain columns with the same names as the original data.}
  \item{\dots}{Other options.}
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{FAMD}}}

