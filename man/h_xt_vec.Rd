\name{h_xt_vec}
\title{Hqm estimator on the marker grid}
\alias{h_xt_vec}
\description{
Computes the hqm estimator on the marker grid.
}

\usage{
h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
}

\arguments{
\item{br_X}{Marker value grid points that will be used in the evaluatiuon.}
\item{br_s}{Time value grid points that will be used in the evaluatiuon.}
\item{size_s_grid}{Size of the time grid.}
\item{alpha}{Marker-hazard obtained from \code{\link{get_alpha}}.}
\item{t}{Numeric value of the time the function should be evaluated.}
\item{b}{Bandwidth.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{n}{Number of individuals.}
}

\details{
The function implements the future conditional hazard estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s},}
for every \eqn{x} on the marker grid where \eqn{X} is the marker, \eqn{Z} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details.
}

\value{
A vector of \eqn{\hat{h}_{x}(t)} for all values \eqn{x} on the marker grid.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



%\seealso{ \code{\link{VarBandHazEst}, \link{TransHazRateEst}, \link{PlugInBand}} }

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

b = 1.7
alpha<-get_alpha(N, Y, b, br_X, K=Epan )

Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s,
              size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)

t = 2

h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)


#Single even per individual example: lung cancer data example 
library(survival) #load the data from the survival package

size_s_grid <- size_X_grid <- 100
n=nrow(lung)
s = lung$time
X = lung$age
XX = lung$age
ss <- lung$time
delta <- lung$status -1 #transform censoring indicators to 0/1 format


br_s = seq(0, max(s), max(s)/( size_s_grid-1))
br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
X_lin = lin_interpolate(br_s, 1:n, 1:n, X, s)
int_X <- findInterval(X_lin, br_X)
int_s = rep(1:length(br_s), n)

N <- make_N(lung, lung, br_X, br_s, ss, XX, delta)
Y <- make_Y(lung, lung, X_lin, br_X, br_s,
            size_s_grid, size_X_grid, int_s, int_X, event_time = 'time', n)

b =  5 
alpha<-get_alpha(N, Y, b, br_X, K=Epan )

Yi <- make_Yi(lung, lung, X_lin, br_X, br_s,
              size_s_grid, size_X_grid, int_s, int_X, event_time = 'time', n)

t =  60
#Calculate hazard rate function
HR.fun<-h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n) 
#Calculate survival function
Surv.fun<- make_sf(20, HR.fun)
#plot the result  
plot(br_s, Surv.fun, type="l" )


}
