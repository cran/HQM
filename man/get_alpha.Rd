\name{get_alpha}
\title{Marker-only hazard rate}
\alias{get_alpha}
\description{
Calculates the marker-only hazard rate for time dependent data.
}

\usage{
get_alpha(N, Y, b, br_X, K=Epan )
}

\arguments{
\item{N}{A matrix made by \code{\link{make_N}} indicating the occurences of events.}
\item{Y}{A matrix made by \code{\link{make_Y}} indicating the exposure. }
\item{b}{Bandwidth.}
\item{br_X}{Vector of grid points for the marker values \eqn{X}.}
\item{K}{Used kernel function.}
}

\details{
The function \code{\link{get_alpha}} implements the marker-only hazard estimator
\deqn{\hat{\alpha}_i(z) = \frac{\sum_{k\neq i}\int_0^T K_{b_1}(z-X_k(s))\mathrm{d}N_k(s)}{\sum_{k\neq i}\int_0^T K_{b_1}(z-X_k(s))Z_k(s)\mathrm{d}s},}
where \eqn{X} is the marker and \eqn{Z} is the exposure. The marker-only hazard is defined as the underlying hazard which is not dependent on time
\deqn{\alpha(X(t),t) = \alpha(X(t)).}

}

\value{
A vector of marker-only values for \code{br_X}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{h_xt}} }

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

N  <- make_N(pbc2, pbc2_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid,
            size_X_grid, int_s, int_X, event_time = 'years', n)

b = 1.7
alpha<-get_alpha(N, Y, b, br_X, K=Epan )
}
