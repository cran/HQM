\name{lin_interpolate_plus}
\alias{lin_interpolate_plus}
\title{Interpolate marker values onto a time grid}
\description{Interpolate marker values at grid times for each subject id, slightly adjusted version of \code{lin_interpolate}.}
\usage{lin_interpolate_plus(t, i, data_id, data_marker, data_time)}
\arguments{
  \item{t}{Numeric vector of target times.}
  \item{i}{Vector of ids for which to compute interpolation.}
  \item{data_id}{Vector of ids aligned with observations.}
  \item{data_marker}{Observed marker values.}
  \item{data_time}{Observation times aligned with \code{data_marker}.}
}
\value{Matrix of interpolated values.}

\examples{
size_s_grid <- 100
X = pbc2$serBilir
s = pbc2$year
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
pbc2_id = to_id(pbc2)

X_lin = lin_interpolate_plus(br_s, pbc2_id$id, pbc2$id, X, s)

}
