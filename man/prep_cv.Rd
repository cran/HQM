\name{prep_cv}
\title{Prepare for Cross validation bandwidth selection}
\alias{prep_cv}
\description{
Implements the calculation of the hqm estimator on cross validation data sets. This is a preparation for the cross validation bandwidth selection technique for future conditional hazard rate estimation based on marker information data.
}

\usage{
prep_cv(data, data.id, marker_name, event_time_name = 'years',
        time_name = 'year',event_name = 'status2', n, I, b)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{data.id}{An id data frame obtained from \code{\link{to_id}}.}
\item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{n}{Number of individuals.}
\item{I}{Number of observations leave out for a K cross validation.}
\item{b}{Bandwidth.}
}

\details{
The function splits the data set via \code{\link{dataset_split}} and calculates for every splitted data set the hqm estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s},}
for all \eqn{x} on the marker grid and \eqn{t} on the time grid, where \eqn{X} is the marker, \eqn{Z} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details.
}

\value{
A list of matrices for every cross validation data set with \eqn{\hat{h}_x(t)} for all \eqn{x} on the marker grid and \eqn{t} on the time grid.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{b_selection}} }

\examples{
\donttest{
pbc2_id = to_id(pbc2)
n = max(as.numeric(pbc2$id))
b = 1.5
I = 26
h_xt_mat_list = prep_cv(pbc2, pbc2_id, 'serBilir', 'years', 'year', 'status2', n, I, b)
}}
