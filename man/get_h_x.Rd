\name{get_h_x}
\title{Future conditional hazard rate for all time values}
\alias{get_h_x}
\description{
Calculates the future conditional hazard rate for a marker value \code{x} and all possible time values.
}

\usage{
get_h_x(data, marker_name, event_time_name = 'years',
        time_name = 'year', event_name = 'status2', x, b)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{x}{Numeric value of the last observed marker value.}
\item{b}{Bandwidth.}
}

\details{
The function \code{\link{h_xt}} implements the future conditional hazard estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s},}
where \eqn{X} is the marker, \eqn{Z} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details.
}

\value{
A vector of \eqn{\hat h_x(t)} for a grid of possible time values \eqn{t}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{get_alpha}, \link{h_xt} }}

\examples{
b = 10
x = 3

get_h_x(pbc2, 'serBilir', event_time_name = 'years',
        time_name = 'year', event_name = 'status2', x, b)
}
