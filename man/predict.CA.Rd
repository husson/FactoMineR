\name{predict.CA}

\alias{predict.CA}

\title{Predict projection for new rows with Correspondence Analysis}

\description{
Predict the projection of new rows with Correspondence Analysis.
}

\usage{
\method{predict}{CA}(object, newdata, \dots)
}

\arguments{
  \item{object}{an object of class CA}
  \item{newdata}{A data frame or a matrix in which to look for variables with which to predict. newdata must contain columns with the same names as the original data.}
  \item{\dots}{Other options.}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{CA}}}

