\name{Epan}
\alias{Epan}


\title{Epanechnikov kernel}

\description{Implements the Epanechnikov kernel function}

\usage{
Epan(x)
}

\arguments{
\item{x}{ A vector of design points where the kernel will be evaluated.}
}

\details{
Implements the   Epanechnikov kernel function
\deqn{ K(x) = \frac{3}{4}(1-x^2)*(|x|<1)),}
}

\value{
Scalar, the value of the Epanechnikov kernel at \eqn{x}.
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
