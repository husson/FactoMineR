\name{predict.PCA}

\alias{predict.PCA}

\title{Predict projection for new rows with Principal Component Analysis}

\description{
Predict the projection of new rows with Principal Component Analysis.
}

\usage{
\method{predict}{PCA}(object, newdata, \dots)
}

\arguments{
  \item{object}{an object of class PCA}
  \item{newdata}{A data frame or a matrix in which to look for variables with which to predict. newdata must contain columns with the same names as the original data.}
  \item{\dots}{Other options.}
}

\author{Francois Husson \email{francois.husson@institut-agro.fr}}

\seealso{ \code{\link{PCA}}}

