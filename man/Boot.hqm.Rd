\name{Boot.hqm}
\alias{Boot.hqm}
\title{Indexed HQM hazard estimator for one bootstrap sample}
\description{Compute the bootstrap estimate of the indexed HQM hazard estimate for a single (bootstrap) sample. The function is meant to be used 
              internally for the calculation of confidence intervals}
\usage{Boot.hqm(in.par, data, data.id, X1, XX1, X2, XX2, event_time_name = 'years', 
                 time_name = 'year', event_name = 'status2', b, t)}
\arguments{
  \item{in.par}{Numeric vector, the values of the indexing parameters.}
  \item{data}{Bootstrap data.frame.}
  \item{data.id}{Id-level data.frame (result of to_id).}
  \item{X1,XX1,X2,XX2}{Vectors, longitudinaal and id level marker values for indexing.}
  \item{event_time_name}{Name of event time column.}
  \item{time_name}{Name of observation time column.}
  \item{event_name}{Name of event indicator column.}
  \item{b}{Bandwidth parameter.}
  \item{t}{Conditioning level.}
}
\value{Numeric vector with estimated hazard on the grid.}


\examples{
\donttest{
#Single instance of the bootstrap version of the bootstrap version
#of the indexed HQM estimator

b.alb = 0.9   
b.bil = 4

t.alb = 1 # refers to zero mean variables - slightly high
t.bil = 1.9 # refers to zero mean variable - high

par.alb  <- 0.0702 #0.149
par.bil <- 0.0856 #0.10

 
b = 0.42 # The result, on the indexed marker 'indmar' of 
         #\code{b_selection(pbc2, 'indmar', 'years', 'year', 'status2', I=26, seq(0.2,0.4,by=0.01))} 
t = t.alb * par.alb + t.bil *par.bil

marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'

data.use<-pbc2
data.use.id<-to_id(data.use)
data.use.id<-data.use.id[complete.cases(data.use.id), ]

# mean adjust the data:
X1=data.use[,marker_name1] -mean(data.use[, marker_name1])
XX1=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
X2=data.use[,marker_name2]  -mean(data.use[, marker_name2])
XX2=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])

boot.haz<-Boot.hqm (c(par.alb,par.bil), data.use, data.use.id, X1, XX1, X2, XX2,  
                    event_time_name = 'years', time_name = 'year', event_name = 'status2',   b, t)

}
}
