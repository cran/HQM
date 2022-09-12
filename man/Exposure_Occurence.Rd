\name{make_N, make_Ni, make_Y, make_Yi}
\title{Occurance and Exposure on grids}
\alias{make_N}
\alias{make_Ni}
\alias{make_Y}
\alias{make_Yi}

\description{
Auxiliary functions that help automate the process of calculating integrals with occurances or exposure processes.
}


\usage{
make_N(data, data.id, breaks_X, breaks_s, ss, XX, delta)
make_Ni(breaks_s, size_s_grid, ss, delta, n)
make_Y(data, data.id, X_lin, breaks_X, breaks_s,
        size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)
make_Yi(data, data.id, X_lin, breaks_X, breaks_s,
        size_s_grid, size_X_grid, int_s,int_X, event_time = 'years', n)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{data.id}{An id data frame obtained from \code{\link{to_id}}.}
\item{breaks_X}{Marker value grid points where the function will be evaluated.}
\item{breaks_s}{Time value grid points where the function will be evaluated.}
\item{ss}{Vector with event times.}
\item{XX}{Vector of last observed marker values.}
\item{delta}{0-1 vector of whether events happened.}
\item{size_s_grid}{Size of the time grid.}
\item{size_X_grid}{Size of the marker grid.}
\item{n}{Number of individuals.}
\item{X_lin}{Linear interpolation of observed marker values evaluated on the marker grid.}
\item{int_s}{Position of the observed time values on the time grid.}
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{event_time}{String of the column name with the event times.}
}

\details{Implements matrices for the computation of integrals with occurences and exposures of the form
\deqn{\int f(s)Z(s)Z(s+t)ds, \int f(s) Z(s)ds, \int f(s) dN(s).}
where \eqn{N} is a 0-1 counting process, \eqn{Z} the exposure and \eqn{f} an arbitrary function.
}

\value{
The functions \code{\link{make_N}} and \code{\link{make_Y}} return a matrix on the time grid and marker grid for occurence and exposure, respectively, while \code{\link{make_Ni}} and \code{\link{make_Yi}} return a matrix on the time grid for evey individual again for occurence and exposure, respectively.
}

\seealso{ \code{\link{h_xt}, \link{g_xt}, \link{get_alpha} }}

\examples{
pbc2_id = to_id(pbc2)
size_s_grid <- size_X_grid <- 100
n = max(as.numeric(pbc2$id))
s = pbc2$year
X = pbc2$serBilir
XX = pbc2_id$serBilir
ss <- pbc2_id$years
delta <- pbc2_id$status2
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)

int_X <- findInterval(X_lin, br_X)
int_s = rep(1:length(br_s), n)

N <- make_N(pbc2, pbc2_id, br_X, br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s,
            size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)
Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s,
              size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)
Ni  <- make_Ni(br_s, size_s_grid, ss, delta, n)

}



