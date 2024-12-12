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

\examples{

# Longitudinal data example

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

# Time-invariant data example:
# pbc2 dataset, single event per individual version:
# 312 observations, most recent event per individual.
# Use landmarking to produce comparable curve with KM.
library(survival)
Landmark <- 3 #set the landmark to 3 years
pbc2.use<- to_id(pbc2) # keep only the most recent row per patient
pbcT1 <- pbc2.use[which(pbc2.use$year< Landmark  & pbc2.use$years> Landmark),]

timesS2 <- seq(Landmark,14,by=0.5)
b=0.9
arg1<- get_h_x(pbcT1, 'albumin', event_time_name = 'years',  time_name = 'year', 
                                                    event_name = 'status2', 2, b) 
br_s2  = seq(Landmark, 14,  length=99)
sfalb2<- make_sf(    (br_s2[2]-br_s2[1])/1.35 , arg1)
kma2<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

#Plot the survival functions:
plot(br_s2, sfalb2, type="l", ylim=c(0,1), xlim=c(Landmark,14), ylab="Survival probability", 
            xlab="years",lwd=2, main="HQM and KM survival functions, conditional on albumin=2, 
            for the time-invariant pbc dataset")
lines(kma2$time, kma2$surv, type="s",lty=2, lwd=2, col=2)
legend("bottomleft", c("HQM est.", "Kaplan-Meier"), lty=c(1,2), col=1:2, lwd=2, cex=1.7)

 


}
