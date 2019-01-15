\name{textual}

\alias{textual}

\title{Text mining}

\description{
Calculates the number of occurence of each words and a contingence table
}

\usage{
textual(tab, num.text, contingence.by=1:ncol(tab), 
    maj.in.min = TRUE, sep.word=NULL)
}

\arguments{
  \item{tab}{a data frame with one textual variable}
  \item{num.text}{indice of the textual variable}
  \item{contingence.by}{a list with the indices of the variables for which a contingence table is calculated by default a contingence
  table is calculated for all the variables (except the textual one). A contingence table can also be calculated for couple of variables.
  If \code{contingence.by} is equal to num.text, then the contingence table is calculated for each row of the data table}
  \item{maj.in.min}{boolean, if TRUE majuscule are transformed in minuscule}
  \item{sep.word}{a string with all the characters which correspond to separator of words}
}

\value{
Returns a list including:
  \item{cont.table}{the contingence table with in rows the categories of the categorical variables (or the couple of categories), and in column the words, 
  and in each cell the number of occurence}
  \item{nb.words}{a data.frame with all the words and for each word, the number of lists in which it is present, and the number of occurence}
}

\author{Francois Husson \email{Francois.Husson@agrocampus-ouest.fr}}

\seealso{ \code{\link{CA}}, \code{\link{descfreq}}}

\examples{
data(poison.text)
res.text <- textual(poison.text, num.text = 3, contingence.by = 1)
descfreq(res.text$cont.table)
## Contingence table for the couple of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(c(1,2)))
descfreq(res.text2$cont.table)
## Contingence table for sex, sick and the couple of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(1,2,c(1,2)))
}

\keyword{multivariate}
