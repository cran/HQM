\name{to_id}
\title{Event data frame}
\alias{to_id}
\description{
Creates a data frame with only one entry per individual from a data frame with time dependent data. The resulting data frame focusses on the event time and the last observed marker value.
}

\usage{
to_id(data_set)
}

\arguments{
\item{data_set}{A data frame of time dependent data points.  Missing values are allowed.}
}

\details{
The function \code{\link{to_id}} uses a data frame of time dependent marker data to create a smaller data frame with only one entry per individual, the last observed marker value and the event time. Note that the column indicating the individuals must have the name \code{id}. Note also that this data frame is similar to \code{pbc2.id} from the \code{JM} package with the difference that the last observed marker value instead of the first one is captured.
}

\value{
A data frame with only one entry per individual.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



%\seealso{ \code{\link{VarBandHazEst}, \link{TransHazRateEst}, \link{PlugInBand}} }

\examples{
data_set.id = to_id(pbc2)
}
