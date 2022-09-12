\name{Q1}
\title{Bandwidth selection score Q1}
\alias{Q1}
\description{
Calculates a part for the K-fold cross validation score.
}

\usage{
Q1(h_xt_mat, int_X, size_X_grid, n, Yi)
}

\arguments{
\item{h_xt_mat}{A matrix of the estimator for the future conditional hazard rate for all values \code{x} and \code{t}.}
\item{int_X}{Vector of the position of the observed marker values in the grid for marker values.}
\item{size_X_grid}{Numeric value indicating the number of grid points for marker values.}
\item{n}{Number of individuals.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
}

\details{
The function implements
\deqn{Q_1 =  \sum_{i = 1}^N \int_0^T \int_s^T Z_i(t)Z_i(s)\hat{h}_{X_i(s)}^2(t-s) dt ds,}
where \eqn{\hat{h}} is the hqm estimator, \eqn{Z} the exposure and \eqn{X} the marker.
}

\value{
A value of the score Q1.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{b_selection}} }

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

h_xt_mat = t(sapply(br_s[1:99],
            function(si){h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)}))

Q = Q1(h_xt_mat, int_X, size_X_grid, n, Yi)
}
