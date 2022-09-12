\name{make_sf}
\title{Survival function from a hazard}
\alias{make_sf}
\description{
Creates a survival function from a hazard rate which was calculated on a grid.
}

\usage{
make_sf(step_size_s_grid, haz)
}

\arguments{
\item{step_size_s_grid}{Numeric value indicating the distance between two grid continuous grid points.}
\item{haz}{Vector of hazard values. Hazard rate must have been calculated on a time grid.}
}

\details{
The function \code{\link{make_sf}} calculates the survival function
\deqn{S(t) = \exp (-\int_0^t h(t) dt),}
where \eqn{h} is the hazard rate. Here, a discritisation via an equidistant grid \eqn{\{ t_i \}} on \eqn{[0,t]} is used to calculate the integral and it is assumed that \eqn{h} has been calculated for exactly these time points \eqn{ t_i }.
}

\value{
A vector of values \eqn{S(t_i)}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



%\seealso{ \code{\link{VarBandHazEst}, \link{TransHazRateEst}, \link{PlugInBand}} }

\examples{
make_sf(0.1, rep(0.1,10))
}
