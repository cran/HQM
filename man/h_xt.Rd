\name{h_xt}
\title{Local constant future conditional hazard rate estimation at a single time point}
\alias{h_xt}
\description{
Calculates the (indexed) future conditional hazard rate for a marker value \code{x} and a time value \code{t}.
}

\usage{
h_xt(br_X, br_s, int_X, size_s_grid, alpha, x,t, b, Yi,n)
}

\arguments{
\item{br_X}{Vector of grid points for the marker values \eqn{X}.}
\item{br_s}{Vector of grid points for the time values \eqn{s}.}
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{size_s_grid}{Size of the time grid.}
\item{alpha}{Marker-hazard obtained from \code{\link{get_alpha}}.}
\item{x}{Numeric value of the last observed marker value.}
\item{t}{Numeric time value.}
\item{b}{Bandwidth.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{n}{Number of individuals.}
}

\details{
Function \code{\link{h_xt}}   implements the future conditional hazard estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(\hat \theta^T X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-\hat \theta^T X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x- \hat \theta^T X_i(s))\mathrm {d}s},}
where, for a positive integer \eqn{p},  \eqn{\hat \theta^T = (\hat \theta_1, \dots, \hat \theta_p )}  is the vector of the estimated indexing parameters,  
\eqn{X_i = (X_{1, i}, \dots, X_{i,p})} is a vector of markers for indexing, \eqn{Z_i} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details 
and \eqn{K_b = b^{-1}K(./b)} is an ordinary kernel function, e.g. the Epanechnikov kernel. For \eqn{p=1} and \eqn{\hat \theta = 1} the above estimator becomes the HQM hazard rate estimator conditional on one covariate, 
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s},}
defined in equation (2) of \doi{10.1093/biomet/asaf008}. 

The future conditional hazard is defined as
\deqn{h_{x,T}(t) = P\left(T_i\in (t+T, t+T+dt)| \theta_0^T X_i(T)=x, T_i > t+T\right),}
where \eqn{T_i} is the survival time and \eqn{X_i} the marker of individual \eqn{i} observed in the time frame \eqn{[0,T]}. 
 
}

\value{
A single numeric value of \eqn{\hat h_x(t)}.
}



\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}
}



\seealso{ \code{\link{get_alpha}} }

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

N <- make_N(pbc2, pbc2_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X, 
            'years', n)

b = 1.7
alpha<-get_alpha(N, Y, b, br_X, K=Epan )

Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X,
              'years', n)

x = 2
t = 2
h_hat = h_xt(br_X, br_s, int_X, size_s_grid, alpha, x, t, b, Yi, n)
}
