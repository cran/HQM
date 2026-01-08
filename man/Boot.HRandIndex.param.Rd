\name{Boot.hrandindex.param}
\alias{Boot.hrandindex.param}
\title{Bootstrap Estimation of Hazard Function and Index Parameters}
\description{
  Performs bootstrap estimation of hazard rates and corresponding index parameters
  for a given set of bootstrap samples. For each bootstrap replicate, the function
  re-estimates the index parameters via optimisation and computes hazard estimates
  using \code{Boot.hqm}. The output is used as input in functions \code{Quantile.Index.CIs} and \code{Pivot.Index.CIs}
}
\usage{Boot.hrandindex.param(B, Boot.samples, marker_name1, marker_name2,event_time_name, 
                             time_name, event_name, b, t, true.haz, v.param, n.est.points)
}
\arguments{
  \item{B}{Integer. Number of bootstrap iterations.}
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
  \item{n.est.points}{Integer. Number of estimation grid points for the hazard curve.}
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

\value{A matrix  of dimension \code{n.est.points × B} containing the bootstrap hazard estimates.}
 

%\author{
%  Your Name
%}

\seealso{
  \code{\link{Boot.hqm}}, \code{\link{index_optim}}, \code{\link{to_id}}
}
\examples{
\donttest{
marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'

par.x1  <- 0.0702  
par.x2 <- 0.0856  
t.x1 = 0 # refers to zero mean variables - slightly high
t.x2 = 1.9 # refers to zero mean variable - high
b = 0.42 
t = par.x1 * t.x1 + par.x2 *t.x2

# first simulate true HR function:
xin <- pbc2[,c(id, marker_name1, marker_name2, event_time_name, time_name, event_name)]
n <- length(xin$id)
nn<-max(  as.double(xin[,'id']) )
xin.id <- to_id(xin)


#  Create bootstrap samples by group:
set.seed(1)  
B<- 10 # 400 #50
Boot.samples<-list()
for(j in 1:B)
{
  i.use<-c()
  id.use<-c()
  index.nn <- sample (nn, replace = TRUE)  
  for(l in 1:nn)
  {
    i.use2<-which(xin[,id]==index.nn[l])
    i.use<-c(i.use, i.use2)
    id.use2<-rep(index.nn[l], times=length(i.use2))
    id.use<-c(id.use, id.use2)
  }
  xin.i<-xin[i.use,]
  xin.i<-xin[i.use,]
  Boot.samples[[j]]<- xin.i[order(xin.i$id),] #xin[i.use,]
}
true.hazard<- Sim.True.Hazard(Boot.samples, id='id', 100,  marker_name1=marker_name1,  
                    marker_name2= marker_name2, event_time_name = event_time_name, 
                    time_name = time_name,  event_name = event_name, 
                    in.par = c(par.x1,  par.x2), b)
                              
res <- Boot.hrandindex.param( B,  Boot.samples, marker_name1, marker_name2, 
          event_time_name,  time_name,  event_name ,  b = 0.4, t = 1.0,  
          true.haz = true.hazard, v.param = c(0.07, 0.08), n.est.points = 100)
          
#return bootstrap hazard rate estimators in marix format:
res
}
}