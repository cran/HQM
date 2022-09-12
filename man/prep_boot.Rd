\name{prep_boot}
\title{Precomputation for wild bootstrap}
\alias{prep_boot}
\description{
Implements key components for the wild bootstrap of the hqm estimator in preparation for obtaining confidence bands.
}

\usage{
prep_boot(g_xt, alpha, Ni, Yi, size_s_grid, br_X, br_s, t, b, int_X, x, n)
}

\arguments{
\item{g_xt}{A vector obtained by \code{\link{g_xt}}.}
\item{alpha}{A vector of the marker only hazard on the marker grid obtained by \code{\link{get_alpha}}.}
\item{Ni}{A matrix made by \code{\link{make_Ni}} indicating the occurence.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{size_s_grid}{Size of the time grid.}
\item{br_X}{Vector of grid points for the marker values.}
\item{br_s}{Time value grid points that will be used in the evaluatiuon.}
\item{t}{Numeric value of the time the function should be evaluated.}
\item{b}{Bandwidth.}
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{x}{Numeric value of the last observed marker value.}
\item{n}{Number of individuals.}
}

\details{
The function implements
\deqn{A_B(t)  =  \frac{1}{\sqrt{n}} \sum_{i=1}^n \int^{T}_0 \hat{g}_{i,t,x_*}(X_i(s))  V_i\{dN_i(s) - \hat{\alpha}_i(X_i(s))Z_i(s)ds\},}
and
\deqn{
B_B(t)  = \frac{1}{\sqrt{n}}\sum_{i = 1}^n V_i\{\hat{\Gamma}(t,x_*)^{-1}W_i(t,x_*) - \hat{h}_{x_*}(t)\},}
where \eqn{V \sim N(0,1)},
\deqn{W_i(t) =\int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_b(x_*,X_i(s))\mathrm {d}s,}
and
\deqn{\hat{\Gamma}(t,x)  = \frac{1}{n}  \sum_{i = 1}^n \int_{0}^{T-t} Z_i(t+s)Z_i(s) K_b(x,X_i(s))ds,}
with \eqn{Z} being the exposure and \eqn{X} the marker.
}

\value{
A list of 5 items. The first two are vectors for calculating \eqn{A_B} and the third one a vector for \eqn{B_B}. The 4th one is the value of the hqm estimator that can also be obtained by \code{\link{h_xt}} and the last one is the value of \eqn{\Gamma}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{Conf_bands}} }

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
Ni  <- make_Ni(br_s, size_s_grid, ss, delta, n)

t = 2
x = 2

g = g_xt(br_X, br_s, size_s_grid, int_X, x, t, b, Yi, Y, n)

Boot_all = prep_boot(g, alpha, Ni, Yi, size_s_grid, br_X, br_s, t, b, int_X, x, n)
Boot_all
}
