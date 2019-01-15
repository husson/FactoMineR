\name{tab.disjonctif.prop}

\alias{tab.disjonctif.prop}

\title{Make a disjunctive table when missing values are present}

\description{
Create a disjunctive table. The missing values are replaced by the proportion of the category.
}

\usage{
tab.disjonctif.prop(tab,seed=NULL,row.w=NULL)
}

\arguments{
  \item{tab}{a data frame with factors}
  \item{row.w}{an optional row weights (by default, a vector of 1 for uniform row weights)}
  \item{seed}{a single value, interpreted as an integer for the set.seed function (if seed = NULL, missing values are initially imputed by the mean of each variable)}
}

\value{The disjonctif table.prop}

\keyword{algebra}
