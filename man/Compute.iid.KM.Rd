\name{Compute.iid.KM}
\title{Compute.iid.KM function implementation}
\alias{Compute.iid.KM}
\description{
Calculates the AUC for the HQM estimator.
}

\usage{Compute.iid.KM(times,status) }

\arguments{
\item{times}{The vector of (censored) event-times.}
\item{status}{The vector of event indicators at the corresponding value of the vector T. Censored observations must be denoted by the value 0. }
}

\details{
 test
}

\value{
A vector of two values: the landmark time of the calculation and the AUC value.
}




\seealso{ \code{\link{bs.hqm} }}

 
