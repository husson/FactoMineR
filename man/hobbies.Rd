\name{hobbies}

\alias{hobbies}

\docType{data}

\title{hobbies (data)}

\description{
The data used here concern a questionnaire on hobbies.
We asked to 8403 individuals how answer questions about their hobbies (18 questions). The following four variables were used to label the individuals: sex (male, female), age (15-25, 26-35, 36-45, 46-55, 56-65, 66-75, 76-85, 86-100), marital status (single, married, widowed,divorced, remarried), profession (manual labourer, unskilled worker, technician, foreman, senior management, employee, other). And finally, a quantitative variable indicates the number of hobbies practised out of the 18 possible choices.
}

\usage{data(tea)}

\format{
A data frame with 8403 rows and 23 columns. Rows represent the individuals,
columns represent the different questions. The first 18 questions are active ones,
 and the 4 following ones are supplementary categorical variables and the 23th is a supplementary quantitative variable (the number of activities)
}

\examples{
data(hobbies)
\dontrun{
res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23,method="Burt")
plot(res.mca,invisible=c("ind","quali.sup"),hab="quali") ### active var. only
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none") ### individuals only
plot(res.mca,invisible=c("ind","var"),hab="quali") ### supp. qualitative var. only

dimdesc(res.mca)
plotellipses(res.mca,keepvar=1:4)
}

}

\keyword{datasets}
