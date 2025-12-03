\name{prep_cv2}
\title{Prepare for Cross validation index parameter selection}
\alias{prep_cv2}
\description{
Implements the calculation of the hqm estimator on cross validation data sets. This is a preparation for the cross validation index selection technique for future conditional hazard rate estimation based on marker information data.
}

\usage{
prep_cv2(in.par, data, data.id, marker_name1, marker_name2, event_time_name = 'years',
        time_name = 'year',event_name = 'status2', n, I, b)
}

\arguments{
\item{in.par}{Vector of candidate indexing values.}
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{data.id}{An id data frame obtained from \code{\link{to_id}}.}
\item{marker_name1}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{marker_name2}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{n}{Number of individuals.}
\item{I}{Number of observations leave out for a K cross validation.}
\item{b}{Bandwidth.}
}

\details{
The function splits the data set via \code{\link{dataset_split}} and calculates for every splitted data set the hqm estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(\theta_0^T X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-\theta_0^T X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-\theta_0^T X_i(s))\mathrm {d}s},}
for all \eqn{x} on the marker grid and \eqn{t} on the time grid, where \eqn{X} is the marker, \eqn{Z} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details.
}

\value{
A list of matrices for every cross validation data set with \eqn{\hat{h}_x(t)} for all \eqn{x} on the marker grid and \eqn{t} on the time grid.
}
 
\seealso{ \code{\link{b_selection_index_optim}} }

 