\name{get_h_xll}
\title{Local linear future conditional hazard rate estimator}
\alias{get_h_xll}
\description{
Calculates the local linear future hazard rate function, conditional on a marker value \code{x}, across a set of time values \code{t}.
}

\usage{ get_h_xll(data, marker_name, event_time_name, time_name, event_name, x, b)}

\arguments{
  \item{data}{A data frame of time dependent data points.  Missing values are allowed.}
  \item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
  \item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
  \item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
  \item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
  \item{x}{Numeric value of the last observed marker value.}
  \item{b}{Bandwidth parameter.}
}

\details{
  The function \code{\link{get_h_xll}} implements the local linear future conditional hazard estimator
  \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-X_i(s))\mathrm {d}s},}
across a grid of possible time values \eqn{t}, where \eqn{X} is the marker, \eqn{Z} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details.
}

\value{
  A vector of \eqn{\hat h_x(t)} for a grid of possible time values \eqn{t}.
}

%\references{
  %\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
  %}



\seealso{ \code{\link{get_alpha}, \link{h_xt} }}

\examples{
library(survival)
b = 10
x = 3
Landmark <- 2
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
b=0.9

arg1ll<-get_h_xll(pbcT1, 'albumin', event_time_name = 'years', time_name = 'year',
                  event_name = 'status2', 2, 0.9) 
arg1lc<-get_h_x(pbcT1, 'albumin', event_time_name = 'years', time_name = 'year',
                event_name = 'status2', 2, 0.9) 

#Caclulate the local contant and local linear survival functions
br_s  = seq(Landmark, 14,  length=99)
sfalb2ll<- make_sf(    (br_s[2]-br_s[1])/4 , arg1ll)
sfalb2lc<- make_sf(    (br_s[2]-br_s[1])/4 , arg1lc)

#For comparison, also calculate the Kaplan-Meier
kma2<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

#Plot the survival functions:
plot(br_s, sfalb2ll,  type="l", col=1, lwd=2, ylab="Survival probability", xlab="Marker level")
lines(br_s, sfalb2lc,  lty=2, lwd=2, col=2)
lines(kma2$time, kma2$surv, type="s",  lty=2, lwd=2, col=3)

legend("topright", c(  "Local linear HQM", "Local constant HQM", "Kaplan-Meier"), 
        lty=c(1, 2, 2), col=1:3, lwd=2, cex=1.7)
}
