\name{index_optim}
\alias{index_optim}
\title{Indexing parameter objective function}
\description{Objective used for optimizing indexing parameters via \code{optim}.}
\usage{index_optim(in.par, data, data.id, br_s, X1, XX1,  event_time_name = 'years', 
            time_name = 'year', event_name = 'status2', b, t, true.haz)}
\arguments{
  \item{in.par}{Numeric vector length 2.}
  \item{data}{Data frame.}
  \item{data.id}{Id-level data frame.}
   \item{br_s}{user-supplied grid points ranging from the minimum to the maximum of the time_name argument}
  \item{X1}{List of vectors for indexing: each vector corresponds to a biomarker and contains one summary measurement per individual.}
  \item{XX1}{List of vectors for indexing: each vector corresponds to a single biomarker and contains its longitudinal measurements across all individuals/time points.}
  \item{event_time_name}{Name of event time column.}
  \item{time_name}{Name of observation time column.}
  \item{event_name}{Name of event indicator column.}
  \item{b}{Bandwidth.}
  \item{t}{Evaluation point or grid.}
  \item{true.haz}{Numeric vector of true hazard values for CV.}
}
\value{Scalar cross-validation score.}


\examples{
 
\donttest{
marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'


par.x1  <- 0.0702 #0.149
par.x2 <- 0.0856 #0.10
t.x1 = 0 # refers to zero mean variables - slightly high
t.x2 = 1.9 # refers to zero mean variable - high
b = 0.42 
t = par.x1 * t.x1 + par.x2 *t.x2


# first simulate true HR function:
xin <- pbc2[,c(id, marker_name1, marker_name2, event_time_name, time_name, event_name)]
ls<-50 # 50 grid points to evaluate the estimates
s.out<- xin[, 'year']
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))

n <- length(xin$id)
nn<-max(  as.double(xin[,'id']) )
###################  Create bootstrap samples by group ####################
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
true.hazard<- Sim.True.Hazard(Boot.samples, id='id', size_s_grid=ls, 
              marker_name1=marker_name1, marker_name2= marker_name2, 
              event_time_name = event_time_name, 
              time_name = time_name,  event_name = event_name, 
              in.par = c(par.x1,  par.x2), b)
##########################################################################

# Then run the optimization for the indexing parameters:

data.use<-Boot.samples[[1]]
data.use.id<-to_id(data.use)
data.use.id<-data.use.id[complete.cases(data.use.id), ]
X1t=data.use[,marker_name1] -mean(data.use[, marker_name1])
XX1t=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
X2t=data.use[,marker_name2]  -mean(data.use[, marker_name2])
XX2t=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])

X1=list(X1t, X2t)
XX1=list(XX1t, XX2t)

s.out<- data.use[, time_name]
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))
res<- optim(par=c(par.x1, par.x2), fn=index_optim,  data=data.use, data.id=data.use.id, 
            br_s=s.out.use, X1=X1, XX1=XX1,  event_time_name = event_time_name, 
            time_name = time_name, event_name = event_name, b=b, t=t, 
            true.haz=true.hazard, method="Nelder-Mead") 
}
 
}