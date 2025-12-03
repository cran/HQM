\name{g_xt}
\title{Computation of a key component for wild bootstrap}
\alias{g_xt}
\description{
Implements a key part for the wild bootstrap of the hqm estimator.
}

\usage{
g_xt(br_X, br_s, size_s_grid, int_X, x, t, b, Yi, Y, n)
}

\arguments{
\item{br_X}{Marker value grid points that will be used in the evaluatiuon.}
\item{br_s}{Time value grid points that will be used in the evaluatiuon.}
\item{size_s_grid}{Size of the time grid.}
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{x}{Numeric value of the last observed marker value.}
\item{t}{Numeric value of the time the function should be evaluated.}
\item{b}{Bandwidth.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{Y}{A matrix made by \code{\link{make_Y}} indicating the exposure. }
\item{n}{Number of individuals.}
}

\details{
The function implements
\deqn{\hat{g}_{t,x}(z) =  \frac{1}{n}  \sum_{ j = 1}^n \int^{T-t}_0 \hat{E}(X_j(t+s))^{-1} K_b(z,X_j(t+s)) Z_j(t+s)Z_j(s)K_b(x,X_j(s))ds,}
for every value \eqn{z} on the marker grid, where \eqn{\hat{E}(x) = \frac{1}{n} \sum_{j=1}^n \int_0^T K_b(x,X_j(s))Z_j(s)ds}, \eqn{Z} the exposure and \eqn{X} the marker.
}

\value{
A vector of \eqn{\hat{g}_{t,x}(z)} for all values \eqn{z} on the marker grid.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



%\seealso{ \code{\link{VarBandHazEst}, \link{TransHazRateEst}, \link{PlugInBand}} }

\examples{
pbc2_id = to_id(pbc2)
size_s_grid <- size_X_grid <- 100
n = max(as.numeric(pbc2$id))
X = pbc2$serBilir
s = pbc2$year
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)

int_X <- findInterval(X_lin, br_X)
int_s = rep(1:length(br_s), n)

Yi<-make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X, 
            'years', n)
Y<-make_Y(pbc2, pbc2_id, X_lin, br_X, br_s,size_s_grid,size_X_grid, int_s,int_X, 
            'years', n)

t = 2
x = 2
b = 10

g_xt(br_X, br_s, size_s_grid, int_X, x, t, b, Yi, Y, n)
}
