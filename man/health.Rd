\name{health}

\alias{health}

\title{health (data)}

\description{
In 1989-1990 the Valencian Institute of Public Health (IVESP) conducted a survey to better know the attitudes and opinions related to health for the non-expert population. The first question included in the questionnaire "What does health mean to you?" required free and spontaneous answers. A priori, the variables Age group (under 21, 21-35, 36-50 and over 50), Health condition (poor, fair, good and very good health) and Gender were considered as possibly conditioning the respondents' viewpoint on health.
}

\usage{
data(health)
}

\format{
A data frame with 392 rows and 118 columns. Rows represent the individuals (respondents), columns represent the words used at least 10 times to answer the open-ended question (columns 1 to 115) and respondents' characteristics (age, health condition and gender)
}

\examples{
\dontrun{
data(health)
res.cagalt<-CaGalt(Y=health[,1:115],X=health[,116:118],type="n")
}
}
\keyword{datasets}
