\name{wine}

\alias{wine}

\docType{data}

\title{Wine}

\description{
The data used here refer to 21 wines of Val de Loire.

}

\usage{data(wine)}

\format{
A data frame with 21 rows (the number of wines) and 31 columns: the first column corresponds to the label of origin,
the second column corresponds to the soil, and the others correspond to sensory
descriptors.
}

\source{
Centre de recherche INRA d'Angers
}

\examples{
data(wine)

## Example of PCA
res.pca = PCA(wine,ncp=5, quali.sup = 1:2)

\dontrun{
## Example of MCA
res.mca = MCA(wine,ncp=5, quanti.sup = 3:ncol(wine))

## Example of MFA
res.mfa = MFA(wine,group=c(2,5,3,10,9,2),type=c("n",rep("s",5)),ncp=5,
    name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6),graph=FALSE)
plotellipses(res.mfa)
plotellipses(res.mfa,keepvar="Label") ## for 1 variable
}
}

\keyword{datasets}
