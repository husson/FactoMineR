\name{poulet}
\alias{poulet}
\docType{data}
\title{Genomic data for chicken}
\description{Genomic data for chicken}
\usage{data(poulet)}
\format{
A data frame with 43 chickens and 7407 variables.  
A factor with levels J16 J16R16 J16R5 J48 J48R24 N\cr
And many continuous variables corresponding to the gene expression
}
\examples{
\dontrun{
data(poulet)
res.pca = PCA(poulet,quali.sup=1, graph=FALSE)
plot(res.pca)
plot(res.pca,habillage=1,label="quali",
    palette=palette(c("black","red","blue","darkgreen","purple","orange")))
dimdesc(res.pca)
## Dessine des ellipses autour des centres de gravite
plotellipses(res.pca)}
}
\keyword{datasets}
