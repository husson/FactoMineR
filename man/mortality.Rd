\name{mortality}

\alias{mortality}

\docType{data}

\title{The cause of mortality in France in 1979 and 2006}

\description{
The cause of mortality in France in 1979 and 2006.
}

\usage{data(mortality)}

\format{
A data frame with 62 rows (the different causes of death) and 18 columns. Each column corresponds to an age interval (15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85-94, 95 and more) in a year. The 9 first columns correspond to data in 1979 and the 9 last columns to data in 2006.
In each cell, the counts of deaths for a cause of death in an age interval (in a year) is given.
}

\source{
Centre d'epidemiologie sur les causes medicales
}

\examples{
data(mortality)

\dontrun{
res<-MFA(mortality,group=c(9,9),type=c("f","f"),
    name.group=c("1979","2006"))

plot(res,choix="freq",invisible="ind",habillage="group")
lines(res$freq$coord[1:9,1],mfa$freq$coord[1:9,2],col="red")
lines(res$freq$coord[10:18,1],mfa$freq$coord[10:18,2],col="green")
    
    }
}

\keyword{datasets}
