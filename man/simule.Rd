\name{simule}

\alias{simule}

\title{Simulate by bootstrap}

\description{Simulate by bootstrap
}

\usage{
simule(data, nb.simul)
}

\arguments{
  \item{data}{A data frame from which the rows are the original data from which the simualte data are calculated (by the average
  of a bootstrap sample. The columns corresponds to the variables for which the simulation should be done. The first column must
  be a factor allowing to group the rows. A bootstrap simulation is done for each level of this factor.}
  \item{nb.simul}{The number of simulations.}
}

\details{
The simulation is independently done for each level of the factor. The number of rows can be different for each levels.

}

\value{
\item{mean}{Data.frame with all the levels of the factor variable, and for each variable, the mean of the original data.}
\item{simul}{Data.frame with all the levels of the factor variable, and for each variable, the nb.simul bootstrap simulations.}
\item{simul.mean}{Data.frame with all the levels of the factor variable, and for each variable, the mean of the simulated data.}
}

\author{Jeremy Mazet}

\keyword{dplot}
