\name{Kernels}
\alias{Epan}
\alias{K_b}
\alias{K_b_mat}

\title{Epanechnikov kernel and pdf kernel estimate}

\description{
Implements the Epanechnikov kernel function and the standard kernel function estimate.
}

\usage{
Epan(x)
K_b(b,x,y, K)
K_b_mat(b,x,y, K)
}

\arguments{
\item{x}{ A vector of design points where the kernel will be evaluated.}
\item{y}{ A vector of sample data points.}
\item{b}{ The bandwidth to use (a scalar).}
\item{K}{ The kernel function to use.}
}

\details{
Implements the   Epanechnikov kernel function
\deqn{ K(x) = \frac{3}{4}(1-x^2)*(|x|<1)),}
and the classical kernel density estimate
\deqn{ \hat{f}(x) = n^{-1}\sum_{i=1}^n K_h(x-X_i).}
}

\value{
In all three cases the functions return the value calculated at \eqn{x}.
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
