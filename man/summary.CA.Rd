\name{summary.CA}

\alias{summary.CA}

\title{Printing summeries of ca objects}
\description{Printing summaries of correspondence analysis objects}

\usage{
\method{summary}{CA}(object, nb.dec = 3, nbelements=10,
   ncp = 3, align.names=TRUE, file="", \dots)
}

\arguments{
  \item{object}{an object of class CA}
  \item{nb.dec}{number of decimal printed}
  \item{nbelements}{number of elements written (rows, columns, ...) ; use \code{nbelements = Inf} if you want to have all the elements}
  \item{ncp}{number of dimensions printed}
  \item{align.names}{boolean, if TRUE the names of the objects are written using the same number of characters}
  \item{file}{a connection, or a character string naming the file to print to}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{CA}}}
