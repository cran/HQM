\name{R_K}
\title{Bandwidth selection score R}
\alias{R_K}
\description{
Calculates a part for the K-fold cross validation score.
}

\usage{
R_K(h_xt_mat_list, int_X, size_X_grid, Yi, Ni, n)
}

\arguments{
\item{h_xt_mat_list}{A list of matrices for all cross validation data sets. Each matrix contains the estimator with the future conditional hazard rate for all values \code{x} and \code{t} and the respected data set.}
\item{int_X}{Vector of the position of the observed marker values in the grid for marker values.}
\item{size_X_grid}{Numeric value indicating the number of grid points for marker values.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{Ni}{A matrix made by \code{\link{make_Ni}} indicating the occurence. }
\item{n}{Number of individuals.}
}

\details{
The function implements the estimator
\deqn{\hat{R}_K =  \sum_{j = 1}^K\sum_{i \in I_j} \int_0^T  g^{-I_j}_i(t)  dN_i(t),}
where \eqn{\hat{g}^{-I_j}_i(t) = \int_0^t Z_i(s) \hat{h}^{-I_j}_{X_i(s)}(t-s) ds,} and \eqn{\hat{h}^{-I_j}} is estimated without information from all counting processes \eqn{i} with \eqn{i \in I_j}.
This function estimates
\deqn{R =  \sum_{i = 1}^N \int_0^T \int_s^T Z_i(t)Z_i(s)\hat{h}_{X_i(s)}(t-s) h_{X_i(s)}(t-s) dt ds .}
where \eqn{\hat{h}} is the hqm estimator, \eqn{Z} the exposure and \eqn{X} the marker.
}

\value{
A matrix with \eqn{\hat{g}^{-I_j}_i(t)} for all individuals \code{i} and time grid points \code{t}.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{b_selection}} }

\examples{
pbc2_id = to_id(pbc2)
n = max(as.numeric(pbc2$id))
b = 1.5
I = 104
h_xt_mat_list = prep_cv(pbc2, pbc2_id, 'serBilir', 'years', 'year', 'status2', n, I, b)


size_s_grid <- size_X_grid <- 100
s = pbc2$year
X = pbc2$serBilir
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

ss <- pbc2_id$years
delta <- pbc2_id$status2

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)
int_X <- findInterval(X_lin, br_X)
int_s = rep(1:length(br_s), n)

Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s,
              size_s_grid, size_X_grid, int_s, int_X, 'years', n)
Ni  <- make_Ni(br_s, size_s_grid, ss, delta, n)

R = R_K(h_xt_mat_list, int_X, size_X_grid, Yi, Ni, n)
R
}
