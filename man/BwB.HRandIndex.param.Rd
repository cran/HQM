\name{BwB.HRandIndex.param}
\alias{BwB.HRandIndex.param}
\title{Bootstrap Estimation of Hazard Function and Index Parameters}
\description{Performs bootstrap estimation of hazard rates and their standard deviation at each grid point (double boostrap) together with the corresponding index parameters
  for a given set of bootstrap samples. The output of the function is used as input in \code{StudentizedBwB.Index.CIs}.}
\usage{BwB.HRandIndex.param(B, B1, Boot.samples, marker_name1, marker_name2, 
                            event_time_name, time_name, event_name, b, t, true.haz, 
                            v.param,  hqm.est, id, xin)
}
\arguments{
  \item{B}{Integer. Number of bootstrap samples.}
   \item{B1}{Integer. Number of bootstrap re-samples.}
  \item{Boot.samples}{A list of bootstrap datasets. Each element corresponds to one replicate.}
  \item{marker_name1}{Character string. Name of the first longitudinal marker.}
  \item{marker_name2}{Character string. Name of the second longitudinal marker.}
  \item{event_time_name}{Name of the event time variable in the data.}
  \item{time_name}{Name of the time variable for the longitudinal marker measurements.}
  \item{event_name}{Name of the event indicator variable.}
  \item{b}{Numeric. Bandwidth parameter used in hazard estimation.}
  \item{t}{Numeric. Evaluation point for the conditional hazard.}
  \item{true.haz}{Numeric vector. The true or reference hazard used in the optimisation criterion.}
  \item{v.param}{Numeric vector. Starting values of the indexing parameters for the optimisation of the index coefficients.}
  \item{hqm.est}{HQM estimator on the original sample.}
  \item{id}{label of id variable of dataset.}
  \item{xin}{original sample.}
}
\details{
  For each bootstrap iteration \eqn{k = 1, \dots, B}, the function:
    \enumerate{
      \item Extracts the bootstrap sample \code{data.use}.
      \item Computes centred marker values at the subject and observation level.
      \item Estimates index parameters by minimising \code{index_optim} using \code{optim}.
      \item Computes the bootstrap hazard estimate via \code{Boot.hqm}.
    }
  The outputs are matrices collecting the hazard estimates and estimated index parameter vectors across bootstrap replicates.
}
\value{A list of matrices of dimension \code{n.est.points × B} containing the bootstrap hazard estimates, 
        the logarithm of the hazard rate estimates and and two vectors of the estimate's standard deviations
        at each grid point.}
 
 
\seealso{
  \code{\link{Boot.hqm}}, \code{\link{index_optim}}, \code{\link{to_id}}
}
\examples{
  
# See the example for  function: StudentizedBwB.Index 
 
 
}