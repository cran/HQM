\name{dataset_split}
\title{Split dataset for K-fold cross validation}
\alias{dataset_split}
\description{
Creates multiple splits of a dataset which is then used in the bandwidth selection with K-fold cross validation.
}

\usage{
dataset_split(I, data)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{I}{The number of individuals that should be left out. Optimally, \eqn{K = n/I} should be an integer, where \eqn{n} is the number of individuals.  }
}

\details{
The function \code{\link{dataset_split}} takes a data frame and transforms it into \eqn{K = n/I} data frames with \eqn{I} individuals missing from each data frame. Let \eqn{I_j} be sets of indices with \eqn{\cup_{j=1}^K I_j = \{1,...,n\}}, \eqn{I_k\cap I_j =  \emptyset} and \eqn{|I_j| = |I_k| = I} for all \eqn{j, k \in \{1,...,K\}}. Then data frames with \eqn{\{1,...,n \}/I_j} individuals are created.
}

\value{
A list of data frames with \code{\link{I}} individuals missing in the above way.
}

%\references{
%\href{https://projecteuclid.org/download/pdf_1/euclid.aos/1176346265}{Tanner and Wong (1983), The Estimation Of The Hazard Function From Randomly Censored Data By The Kernel Method, Annals of Statistics,  3, pp. 989-993.}
%}



\seealso{ \code{\link{b_selection} }}

\examples{
splitted_dataset = dataset_split(26, pbc2)
}
