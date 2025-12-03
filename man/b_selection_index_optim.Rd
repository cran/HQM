\name{b_selection_index_optim}
\title{Cross validation index parameter selection}
\alias{b_selection_index_optim}
\description{
Implements the index parameter selection for two markers based on K-fold cross validation.
}

\usage{
b_selection_index_optim(in.par, data, marker_name1, marker_name2,  
          event_time_name = 'years', time_name = 'year', event_name = 'status2', I, b)
}

\arguments{
\item{in.par}{Vector of candidate values for the index parameters.}
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{marker_name1}{The column name of the first marker values in the data frame \code{\link{data}}. }
\item{marker_name2}{The column name of the second marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{I}{Number of observations leave out for a K cross validation.}
\item{b}{scalar bandwidth for the HQM estimator.}
}

\details{
The function \code{\link{b_selection_index_optim}} implements the cross validation index parameter selection for the indexing of two markers and  given by
\deqn{ \hat \theta = arg min_{\theta_1, \theta_2} \sum_{i = 1}^N \int_0^T \int_s^T Z_i(t)Z_i(s)(\hat{h}_{\theta_0^T X_i(s)}(t-s)- h_{\theta_0^T X_i(s)}(t-s))^2 dt ds,}
where \eqn{\hat h_x(t)} is the HQM estimator of \eqn{ h_x(t)}, see \doi{10.1093/biomet/asaf008}, and \eqn{Z_i} the exposure process of individual \eqn{i}. Note that \eqn{\hat h_x(t)} is dependent on \eqn{b}.
}

\value{
A list with the tested parameters and its cross validation scores.
}


\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}
}


\seealso{ \code{\link{b_selection_prep_g}, \link{Q1}, \link{R_K}, \link{prep_cv}, \link{dataset_split}} }

\examples{

\donttest{
# Obtain the indexing parameters for the combination of albumin and bilirubin markers
# These are the values used in the example of function h_xt_vec
# and yielded the values:  (par.alb, par.bil) = ( 0.0702, 0.0856 ) that were used there.  

I = 26
b_list = seq(1.1, 1.2, by=0.1)

for(i in 1:length(b_list))
{
  res<- optim(c(0.5, 0.5), b_selection_index_optim,  data=pbc2, marker_name1='albumin', 
              marker_name2= 'serBilir', event_time_name = 'years', time_name = 'year', 
              event_name = 'status2', I=26, b=b_list[i], method="Nelder-Mead")
  cat("i= ", i, " ", res$par, " ", res$value, "count calls to fn = ", res$counts, " converge? ", 
        res$convergence,   "\n")
  res
}

}
}
