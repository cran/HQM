\name{SingleIndCondFutHaz}
\alias{SingleIndCondFutHaz}
\title{Local linear future conditional hazard estimator (wrapper)}
\description{Compute the indexed local linear future conditional hazard rate estimator on a grid.}
\usage{SingleIndCondFutHaz(datain, id, marker_name1, marker_name2, event_time_name = 'years', 
       time_name = 'year', event_name = 'status2', in.par, b, t)}
\arguments{
  \item{datain}{Data frame with longitudinal observations.}
  \item{id}{Name of id column (string).}
  \item{marker_name1}{Name of first marker (string).}
  \item{marker_name2}{Name of second marker (string).}
  \item{event_time_name}{Name of event time column.}
  \item{time_name}{Name of observation time column.}
  \item{event_name}{Name of event indicator column.}
  \item{in.par}{Numeric vector length 2 with indexing parameters.}
  \item{b}{Bandwidth parameter.}
   \item{t}{conditioning parameter.}
}
\value{A data frame with columns \code{time} and \code{est}.}

\examples{

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
b = 0.42 # The result, on the indexed marker 'indmar' of 
         #\code{b_selection(pbc2, 'indmar', 'years', 'year', 'status2', I=26, seq(0.2,0.4,by=0.01))} 
t = par.x1 * t.x1 + par.x2 *t.x2

# Calculate the indexed HQM estimator on the original sample:
arg2<-SingleIndCondFutHaz(pbc2, id, marker_name1, marker_name2, event_time_name = 'years', 
      time_name = 'year',  event_name = 'status2', in.par= c(par.x1,  par.x2), b, t)
hqm.est<-arg2[,2]
time.grid<-arg2[,1]
n.est.points<-length(hqm.est)
}
