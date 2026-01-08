\name{Sim.True.Hazard}
\alias{Sim.True.Hazard}
\title{Simulated true hazard (bootstrap average)}
\description{Compute the Monte Carlo / bootstrap averaged alpha (true hazard) across a 
            list of datasets.}
\usage{Sim.True.Hazard(data.use, id, size_s_grid, marker_name1 = marker_name1, 
       marker_name2 = marker_name2, event_time_name = event_time_name, 
       time_name = time_name, event_name = event_name, in.par, b)}
\arguments{
  \item{data.use}{List of data.frames (bootstrap samples).}
  \item{id}{Id column name.}
   \item{size_s_grid}{user supplied grid length on the time_name argument}
  \item{marker_name1}{First marker name.}
  \item{marker_name2}{Second marker name.}
  \item{event_time_name}{Name of event time column.}
  \item{time_name}{Name of observation time column.}
  \item{event_name}{Name of event indicator column.}
  \item{in.par}{Numeric indexing parameters.}
  \item{b}{Bandwidth.}
}
\value{Numeric vector (row means of alpha estimates).}


\examples{

\donttest{ 
marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'

ls<-50 #user supplied grid length on the time_name argument

par.x1  <- 0.0702  #indexing parameter for marker 1
par.x2 <- 0.0856  #indexing parameter for marker 2

t.x1 = 0 # conditioning level for marker 1, assumes zero mean variable
t.x2 = 1.9 # conditioning level for marker 2, assumes zero mean variable
b = 0.42 # The result, on the indexed marker 'indmar' of 
         #\code{b_selection(pbc2,'indmar','years','year','status2',I=26,seq(0.2,0.4,by=0.01))}
t = par.x1 * t.x1 + par.x2 *t.x2

xin <- pbc2[,c(id, marker_name1, marker_name2, event_time_name, time_name, event_name)]
n <- length(xin$id)
nn<-max(  as.double(xin[,'id']) )

#  Create bootstrap samples by group 
set.seed(1)  
B<-100 #set bootstrap iterations here
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
  Boot.samples[[j]]<- xin.i[order(xin.i$id),]  
}

#compute the bootstrap simulated true HR function:
true.hazard<- Sim.True.Hazard(Boot.samples, id='id', size_s_grid=ls, 
              marker_name1=marker_name1, marker_name2= marker_name2, 
              event_time_name = event_time_name, time_name = time_name,  
              event_name = event_name, in.par = c(par.x1,  par.x2), b)

 }

}