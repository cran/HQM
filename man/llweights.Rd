\name{llweights}
\title{Local linear weight functions}
\alias{sn.0}
\alias{sn.1}
\alias{sn.2}
\description{Implements the weights to be used in the local linear HQM estimator.}

\usage{
sn.0(xin, xout, h, kfun)
sn.1(xin, xout, h, kfun)
sn.2(xin, xout, h, kfun)
}

\arguments{
\item{xin}{Sample values.}
\item{xout}{Grid points where the estimator will be evaluated.}
\item{h}{Bandwidth parameter.}
\item{kfun}{Kernel function.}
}

\details{
The function implements the local linear weights in the definition of the estimator \eqn{\hat{h}_x(t)}, see also \code{\link{h_xt}} 
}

\value{ A vector of \eqn{s_n(x)} for all values \eqn{x} on the marker grid. }

   

 