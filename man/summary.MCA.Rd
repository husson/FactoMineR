\name{summary.MCA}

\alias{summary.MCA}

\title{Printing summeries of MCA objects}
\description{Printing summaries of multiple correspondence analysis objects}

\usage{
\method{summary}{MCA}(object, nb.dec = 3, nbelements=10,
    nbind=nbelements, ncp = 3, align.names=TRUE, file="", \dots)
}

\arguments{
  \item{object}{an object of class MCA}
  \item{nb.dec}{number of decimal printed}
  \item{nbelements}{number of elements written (variables, categories, ...);
use \code{nbelements = Inf} if you want to have all the elements}
  \item{nbind}{number of individuals written (individuals and supplementary individuals, ...);
use \code{nbind = Inf} to have the results for all the individuals and \code{nbind = 0} 
if you do not want the results for individuals}
  \item{ncp}{number of dimensions printed}
  \item{align.names}{boolean, if TRUE the names of the objects are written using the same number of characters}
  \item{file}{a connection, or a character string naming the file to print to}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\author{Francois Husson \email{husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{MCA}}}
