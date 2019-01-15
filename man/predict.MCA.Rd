\name{predict.MCA}

\alias{predict.MCA}

\title{Predict projection for new rows with Multiple Correspondence Analysis}

\description{
Predict the projection of new rows with Multiple Correspondence Analysis.
}

\usage{
\method{predict}{MCA}(object, newdata, \dots)
}

\arguments{
  \item{object}{an object of class MCA}
  \item{newdata}{A data frame or a matrix in which to look for variables with which to predict. newdata must contain columns with the same names as the original data.}
  \item{\dots}{Other options.}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MCA}}}

