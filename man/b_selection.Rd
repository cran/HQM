\name{b_selection}
\title{Cross validation bandwidth selection}
\alias{b_selection}
\description{
Implements the bandwidth selection for the future conditional hazard rate \eqn{\hat h_x(t)} based on K-fold cross validation.
}

\usage{
b_selection(data, marker_name, event_time_name = 'years',
            time_name = 'year', event_name = 'status2', I, b_list)
}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{I}{Number of observations leave out for a K cross validation.}
\item{b_list}{Vector of bandwidths that need to be tested.}
}

\details{
The function \code{\link{b_selection}} implements the cross validation bandwidth selection for the future conditional hazard rate \eqn{\hat h_x(t)} given by
\deqn{ b_{CV} = arg min_b \sum_{i = 1}^N \int_0^T \int_s^T Z_i(t)Z_i(s)(\hat{h}_{X_i(s)}(t-s)- h_{X_i(s)}(t-s))^2 dt ds,}
where \eqn{\hat h_x(t)} is a smoothed kernel density estimator of \eqn{ h_x(t)} and \eqn{Z_i} the exposure process of individual \eqn{i}. Note that \eqn{\hat h_x(t)} is dependent on \eqn{b}.
}

\value{
A list with the tested bandwidths and its cross validation scores.
}


\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}
}


\seealso{ \code{\link{b_selection_prep_g}, \link{Q1}, \link{R_K}, \link{prep_cv}, \link{dataset_split}} }

\examples{
\donttest{
I = 26
# For Albumin marker:
b_list = seq(0.9, 1.7, 0.1)

b_scores_alb = b_selection(pbc2, 'albumin', 'years', 'year', 'status2', I, b_list)
b_scores_alb[[2]][which.min(b_scores_alb[[1]])]

# For Bilirubin marker:
b_list = seq(3, 4, 0.1)
b_scores_bil = b_selection(pbc2, 'serBilir', 'years', 'year', 'status2', I, b_list)
b_scores_bil[[2]][which.min(b_scores_bil[[1]])]
b_scores_bil

}
}
