\name{Kernels}
\alias{K_b}
\alias{xK_b}
\alias{K_b_mat}

\title{Classical (unmodified) kernel and related functionals}

\description{
Implements the classical kernel function and related functionals
}

\usage{
K_b(b,x,y, K)
xK_b(b,x,y, K)
K_b_mat(b,x,y, K)
}

\arguments{
\item{x}{ A vector of design points where the kernel will be evaluated.}
\item{y}{ A vector of sample data points.}
\item{b}{ The bandwidth to use (a scalar).}
\item{K}{ The kernel function to use.}
}

\details{
The function \code{K_b} implements the classical kernel function calculation
\deqn{ h^{-1} K \left ( \frac{x-y}{h} \right )}
for scalars \eqn{x} and \eqn{y} while \code{xK_b} implements the functional 
\deqn{ h^{-1} K \left ( \frac{x-y}{h} \right )(x-y)}
again for for scalars \eqn{x} and \eqn{y}. The function \code{K_b_mat} is the vectorized version of \code{K_b}. It uses as inputs the vectors \eqn{(X_1, \dots, X_n)} and \eqn{(Y_1, \dots, Y_n)} and returns a \eqn{n \times n} matrix with entries 
\deqn{ h^{-1} K \left ( \frac{X_i-Y_j}{h} \right )}
}

\value{
Scalar values for \code{K_b} and \code{xK_b} and matrix outputs for \code{K_b_mat}.
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
