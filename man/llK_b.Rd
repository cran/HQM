\name{llK_b}
\alias{llK_b}

\title{Local linear kernel}

\description{
Implements the local linear kernel function.}

\usage{
llK_b(b,x,y, K)
}

\arguments{
\item{x}{ A vector of design points where the kernel will be evaluated.}
\item{y}{ A vector of sample data points.}
\item{b}{ The bandwidth to use (a scalar).}
\item{K}{ The kernel function to use.}
}

\details{
Implements the local linear kernel
\deqn{K_{x,b}(u)= \frac{K_b(u)-K_b(u)u^T D^{-1}c_1}{c_0 - c_1^T D^{-1} c_1}, }
where \eqn{c_1 = (c_{11}, \dots, c_{1d})^T, D = (d_{ij})_{(d+1) \times (d+1)}} with
\deqn{
c_0  = \sum_{i=1}^n \int_0^T K_b(x-X_i(s)) Z_i(s)ds, }
\deqn{
c_{ij}  = \sum_{i=1}^n \int_0^T K_b(x-X_i(s))\{x-X_{ij}(s)\} Z_i(s)ds,}
\deqn{
d_{jk}  =  \sum_{i=1}^n \int_0^T K_b(x-X_i(s))\{x-X_{ij}(s)\}\{x-X_{ik}(s)\} Z_i(s)ds,
}
see also \doi{10.1080/03461238.1998.10413997}.
}

\value{
Matrix output with entries the values of the kernel function at each point.
}


\references{
Nielsen (1998), Marker dependent kernel hazard estimation from local linear estimation, Scandinavian Actuarial Journal,   pp.  113-124.
\doi{10.1080/03461238.1998.10413997}
}
 
 
