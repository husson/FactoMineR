\name{poison.text}

\alias{poison.text}

\docType{data}

\title{Poison}

\description{
The data used here refer to a survey carried out on a sample of children of primary school
who suffered from food poisoning. They were asked about their symptoms and about what they ate.
}

\usage{data(poison)}

\format{
A data frame with 55 rows and 3 columns (the sex, if they are sick or not, and a textual variable with their symptom and what they eat).
}

\examples{
data(poison.text)
res.text <- textual(poison.text, num.text = 3, contingence.by = c(1,2))
## Contingence table for the sex variable, the sich variable and the couple
## of variable sick-sex
res.text2 <- textual(poison.text, num.text = 3, contingence.by = list(1,2,c(1,2)))
}

\keyword{datasets}
