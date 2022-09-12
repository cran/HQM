\name{lin_interpolate}
\title{Linear interpolation}
\alias{lin_interpolate}
\description{
Implements a linear interpolation between observered marker values.
}

\usage{
lin_interpolate(t, i, data_id, data_marker, data_time)
}

\arguments{
\item{t}{A vector of time values where the function should be evaluated.}
\item{i}{A vector of ids of individuals for whom the marker values should be interpolated.}
\item{data_id}{The vector of ids from a data frame of time dependent variables.}
\item{data_marker}{The vector of marker values from a data frame of time dependent variables.}
\item{data_time}{The vector of time values from a data frame of time dependent variables.}
}

\details{
Given time points \eqn{t_1,...,t_K} and marker values \eqn{m_1,...,m_J} at different time points \eqn{t^m_1,...,t^m_J}, the function calculates a linear interpolation \eqn{f} with \eqn{f(t^m_i) = m_i} at the time points \eqn{t_1,...,t_K} for all indicated individuals. Returned are then \eqn{(f(t_1),...,f(t_K))}. Note that the first value is always observed at time point \eqn{0} and the function \eqn{f} is extrapolated constantly after the last observed marker value.
}

\value{
A matrix with columns \eqn{(f(t_1),...,f(t_K))} as described above for every individual in the vector \code{i}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



%\seealso{ \code{\link{VarBandHazEst}, \link{TransHazRateEst}, \link{PlugInBand}} }

\examples{
size_s_grid <- 100
X = pbc2$serBilir
s = pbc2$year
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
pbc2_id = to_id(pbc2)

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)
}
