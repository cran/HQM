\name{SingleIndCondFutHaz}
\alias{SingleIndCondFutHaz}
\title{Local linear future conditional hazard estimator (wrapper)}
\description{Compute the indexed local linear future conditional hazard rate estimator on a grid.}
\usage{SingleIndCondFutHaz(datain, id, ls,  X1, XX1, event_time_name = 'years', 
                            time_name = 'year', event_name = 'status2', in.par, b, t)}
\arguments{
  \item{datain}{Data frame with longitudinal observations.}
  \item{id}{Name of id column (string).}
   \item{ls}{user supplied grid length on the time_name argument.}
  \item{X1}{List of vectors for indexing: each vector corresponds to a biomarker and contains one summary measurement per individual.}
  \item{XX1}{List of vectors for indexing: each vector corresponds to a single biomarker and contains its longitudinal measurements across all individuals/time points.}
  \item{event_time_name}{Name of event time column.}
  \item{time_name}{Name of observation time column.}
  \item{event_name}{Name of event indicator column.}
  \item{in.par}{Numeric vector length 2 with indexing parameters.}
  \item{b}{Bandwidth parameter.}
  \item{t}{conditioning parameter.}
}
\value{A data frame with columns \code{time} and \code{est}.}

\examples{

b.alb = 0.9   
b.bil = 4

t.alb = 1 # refers to zero mean variables - slightly high
t.bil = 1.9 # refers to zero mean variable - high

par.alb  <- 0.0702 #0.149
par.bil <- 0.0856 #0.10


b = 0.42  
t = t.alb * par.alb + t.bil *par.bil

marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'
ls<-50

data.use<-pbc2
data.use.id<-to_id(data.use)
#data.use.id<-data.use.id[complete.cases(data.use.id), ]

# mean adjust the data:
X1t=data.use[,marker_name1] -mean(data.use[, marker_name1])
XX1t=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
X2t=data.use[,marker_name2]  -mean(data.use[, marker_name2])
XX2t=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])

X1=list(X1t, X2t)
XX1=list(XX1t, XX2t)

arg2<-SingleIndCondFutHaz(pbc2, id, ls,  X1, XX1, event_time_name = 'years', 
    time_name = 'year',  event_name = 'status2', in.par= c(par.alb,  par.bil), b, t)
hqm.est<-arg2[,2]
time.grid<-arg2[,1]
n.est.points<-length(hqm.est)
}
