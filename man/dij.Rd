\name{dij}
\alias{dij}

\title{D matrix entries, used for the implementation of the local linear kernel}

\description{Calculates the entries of the \eqn{D} matrix in the definition of the local linear kernel}

\usage{
dij(b,x,y, K)
}

\arguments{
\item{x}{ A vector of design points where the kernel will be evaluated.}
\item{y}{ A vector of sample data points.}
\item{b}{ The bandwidth to use (a scalar).}
\item{K}{ The kernel function to use.}
}

\details{
Implements the  caclulation of all \eqn{d \times d} entries of matrix \eqn{D}, which is part of the definition of the local linear kernel. The actual calculation is performed by
\deqn{
d_{jk}  =  \sum_{i=1}^n \int_0^T K_b(x-X_i(s))\{x-X_{ij}(s)\}\{x-X_{ik}(s)\} Z_i(s)ds,
}
}

\value{
scalar value, the result of  \eqn{d_{jk}}.
}



%\author{
%Dimitrios Bagkavos
%
%R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com>
%}

%\note{
%%  ~~further notes~~
%}

%\seealso{
%\code{\link{alfa.profile}, \link{alfa}, \link{alfainv}, \link{alpha.mle}
%}
%}

%\examples{
%
%}
