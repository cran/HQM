\name{StudentizedBwB.Index.CIs}
\alias{StudentizedBwB.Index.CIs}
\title{Compute Studentized Double Bootstrap Pointwise Confidence Intervals for the Indexed Hazard Rate Estimate}
\description{Computes bootstrap confidence intervals—studentized (double bootstrap) and its symmetric versions for the hazard rate, log-hazard rate, 
              and back-transformed (from the log scale) hazard rate functions, based on the indexed hazard estimator.}
\usage{
  StudentizedBwB.Index.CIs(n.est.points, all.mat, time.grid, hqm.est, a.sig)
}
\arguments{
  \item{n.est.points}{Integer. Number of estimation points at which the indexed hazard estimates and confidence intervals are evaluated.}
  \item{all.mat}{A list of matrices of bootstrap estimated hazard and log hazard rates with dimensions \code{n.est.points × B}, where each column corresponds to one bootstrap replicate.}
  \item{time.grid}{Numeric vector of length \code{n.est.points}: the grid points at which the indexed hazard estimates and confidence intervals are calculated.}
  \item{hqm.est}{Indexed hazard estimator, calculated at the grid points \code{time.grid} and using the original sample.}
  \item{a.sig}{The significance level (e.g., 0.05) which will be used in computing the confidence intervals.}
}
\details{ This function computes several forms of studentized confidence intervals for the indexed hazard rate function.  First, for each bootstrap iteration \eqn{j=1,\dots, B} 
we construct estimators of the standard deviation, denoted by \eqn{\hat \sigma_j^2} as follows: each bootstrap sample is itself bootstrapped, say \eqn{B_1} times, 
we estimate  the corresponding \eqn{B_1} index parameters \eqn{\hat \theta_{j,k} = (\hat \theta_{1,j,k}, \hat \theta_{2,j,k}), k=1,\dots, B_1}, and use them to 
calculate the corresponding hazard estimators \eqn{\hat{h}_{x,}^{(j,k)}(t), k=1,\dots, B_1}.  Finally we calculate \eqn{\hat \sigma_j^2} as the sample variance of 
\eqn{\hat{h}_{x}^{(j,1)}(t),  \dots, \hat{h}_{x}^{(j,B_1)}(t)}. Let \eqn{k_{\alpha/2}^s, k_{1-\alpha/2}^s} and \eqn{\bar k_{1-\alpha}^s} be respectively the 
\eqn{\alpha/2, 1-\alpha/2} and \eqn{1-\alpha} quantiles of
\deqn{
	  \frac{|\hat{h}_{x}^{(j)}(t) - \hat{h}_{x}(t)|}{\hat \sigma_j},\; j=1,\dots, B.
}
 
Given the bootstrap estimators \eqn{\hat{h}_{x}^{(j)}(t), j=1,\dots,B}, the estimator of the original data set  \eqn{\hat{h}_{x}(t)} and the quantiles 
\eqn{k_{\alpha/2}^s, k_{1-\alpha/2}^s} and \eqn{\bar k_{1-\alpha}^s},  the studentized  (double bootstrap) confidence interval (CI) for   \eqn{\hat{h}_{x}(t)} is given by
\deqn{
     \Bigg (  \hat{h}_{x}(t) -  \hat \sigma k_{1-\alpha/2}^s,     \hat{h}_{x}(t) -  \hat \sigma k_{\alpha/2}^s  \Bigg ).  
}
The symmetric studentized   confidence interval (CI) for   \eqn{\hat{h}_{x}(t)}  defined by
\deqn{
\Bigg (  \hat{h}_{x}(t) - \hat \sigma \bar k_{1-\alpha}^s,     \hat{h}_{x}(t) +  \hat \sigma\bar k_{1-\alpha}^s  \Bigg ).   
}
For the confidence intervals for the logarithm of the hazard rate function first set \eqn{	k_{\alpha/2}^{L,s}, k_{1-\alpha/2}^{L,s} } and  \eqn{	\bar k_{1-\alpha}^{L,s}} 
  be the \eqn{\alpha/2, 1-\alpha/2} and \eqn{1-\alpha}  quantile of \eqn{  |\hat{L}_{x}^{(j)}(t) - \hat{L}_{x}(t)|/\hat \sigma_j, j=1,\dots, B}.
 
The studentized (double bootstrap) CI confidence interval for the logarithm of the hazard rate function is
\deqn{
     \Bigg (  \hat{L}_{x}(t) -  \hat \sigma k_{1-\alpha/2}^{L,s},     \hat{L}_{x}(t) -  \hat \sigma k_{\alpha/2}^{L,s}  \Bigg ).   
}
Its symmetric studentized  (double bootstrap) CI for the log hazard is
\deqn{
\Bigg (  \hat{L}_{x}(t) -  \hat \sigma \bar k_{1-\alpha}^{L,s},     \hat{L}_{x}(t) +  \hat \sigma \bar k_{1-\alpha}^{L,s}  \Bigg ).   
}
These confidence intervals are transformed back to yield the following confidence intervals for the hazard rate function \eqn{h_x(t)}: 
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-   \hat \sigma k_{1-\alpha/2}^{L,s}},   \hat{h}_{x}(t) e^{- \hat \sigma   k_{\alpha/2}^{L,s}}  \Bigg ),    
}
and the symmetric version  
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-  \hat \sigma \bar k_{1-\alpha}^{L,s}},     \hat{h}_{x}(t) e^{ \hat \sigma  \bar k_{1-\alpha}^{L,s}}  \Bigg ).    
}

Note: The bootstrap matrix \code{Mat.boot.haz.rate} is assumed to contain estimates  produced using the same time grid as \code{time.grid} and the same estimator used  to generate \code{hqm.est}.
}
\value{
  A data frame with the following columns:
  
  \item{time}{The evaluation grid points.}
  \item{est}{Indexed hazard rate estimator \code{hqm.est}.}
  \item{downci, upci}{Lower and upper endpoints of basic studentized CIs.}
  \item{docisym, upcisym}{Lower and upper endpoints of symmetric CIs.}
  \item{logdoci, logupci}{Lower and upper endpoints of studentized CIs on the log-scale.}
  \item{logdocisym, logupcisym}{Symmetric log-scale CIs.}
  \item{log.est}{The logarithm of the indexed hazard rate estimate, \code{log(hqm.est)}.}
  \item{tLogDoCI, tLogUpCI}{Transformed-log CIs based on \code{2*log(hqm.est) - log-quantiles}.}
  \item{tSymLogDoCI, tSymLogUpCI}{Symmetric transformed-log CIs.}
}
\seealso{
  \code{\link{Boot.hrandindex.param}},  
  \code{\link{Boot.hqm}}
}


%\author{  Your Name }


\examples{
\donttest{ 
marker_name1 <- 'albumin'
marker_name2 <-  'serBilir'
event_time_name <- 'years' 
time_name <- 'year' 
event_name <- 'status2'
id<-'id'

xin <- pbc2[,c(id, marker_name1, marker_name2, event_time_name, time_name, event_name)]
n <- length(xin$id)
nn<-max(  as.double(xin[,'id']) )

xin.id <- to_id(xin)

par.x1  <- 0.0702 #0.149
par.x2 <- 0.0856 #0.10
t.x1 = 0 # refers to zero mean variables - slightly high
t.x2 = 1.9 # refers to zero mean variable - high
b = 0.42#par.alb * b.alb + par.bil *b.bil # 7
t = par.x1 * t.x1 + par.x2 *t.x2
ls<-50

X1t=xin[,marker_name1] -mean(xin[, marker_name1])
XX1t=xin.id[,marker_name1] -mean(xin.id[, marker_name1])
X2t=xin[,marker_name2]  -mean(xin[, marker_name2])
XX2t=xin.id[,marker_name2] -mean(xin.id[, marker_name2])

X1=list(X1t, X2t)
XX1=list(XX1t, XX2t)

# Calculate the indexed HQM estimator on the original sample:
arg2<- SingleIndCondFutHaz(pbc2, id, ls,  X1, XX1, event_time_name = 'years', 
        time_name = 'year',  event_name = 'status2', in.par= c(par.x1,  par.x2), b, t)
                          
hqm.est<-arg2[,2] # Indexed HQM estimator on original sample
time.grid<-arg2[,1] # evaluation grid points
n.est.points<- ls # length(hqm.est)



#  Create bootstrap samples by group 
set.seed(1)  
B<-10 # 20 # 5 for display purposes only; for sensible results use B=200 (slower) 
B1<- 10 # 20  # 5 for display purposes only; use B1=20 (slower)
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
 


# Simulate true hazard rate function:
true.hazard<- Sim.True.Hazard(Boot.samples, id='id', n.est.points, 
              marker_name1=marker_name1, marker_name2= marker_name2, 
              event_time_name = event_time_name, time_name = time_name, 
              event_name = event_name, in.par = c(par.x1,  par.x2), b)

# Bootstrap the original indexed HQM estimator:                              
all.mat.use<-BwB.HRandIndex.param(B, B1, Boot.samples, marker_name1, marker_name2, 
             event_time_name, time_name, event_name, b, t, true.haz=true.hazard, 
             v.param=c(par.x1,  par.x2), hqm.est=hqm.est, id= 'id', xin=xin)
             
# Construct Ci's:  
a.sig<-0.05 
st.ci.data<-StudentizedBwB.Index.CIs(n.est.points, all.mat.use, time.grid, hqm.est, a.sig)

# extract Plain + symmetric CIs
UpCI<-st.ci.data[,"UpCI"]
DoCI<-st.ci.data[,"DoCI"]
SymUpCI<-st.ci.data[,"SymUpCI"]
SymDoCI<-st.ci.data[,"SymDoCI"]

#Plot the selected CIs
J<-80 #select the first 80 grid points (for display purposes only)
plot(time.grid[1:J], hqm.est[1:J], type="l", ylim=c(0,2), ylab="Hazard rate", xlab="time", 
            lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(UpCI[1:J],  rev(DoCI[1:J])), 
      col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  SymDoCI[1:J], lty=2, lwd=2)   

# extract transformed from Log HR + symmetric CIs
LogUpCI<-st.ci.data[,"LogUpCI"]
LogDoCI<-st.ci.data[,"LogDoCI"]
SymLogUpCI<-st.ci.data[,"LogSymUpCI"]
SymLogDoCI<-st.ci.data[,"LogSymDoCI"]

#Plot the selected CI's
plot(time.grid[1:J], log(hqm.est[1:J]), type="l", ylim=c(-5,4), ylab="Log Hazard rate", 
      xlab="time",  lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(LogUpCI[1:J], rev(LogDoCI[1:J])), 
      col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymLogUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  SymLogDoCI[1:J], lty=2, lwd=2)  

# extract Log HR + symmetric CIs  
tLogUpCI<-st.ci.data[,"LogtUpCI"]
tLogDoCI<-st.ci.data[,"LogTDoCI"]
tSymLogUpCI<-st.ci.data[,"SymLogtUpCI"]
tSymLogDoCI<-st.ci.data[,"SymLogTDoCI"]

#Plot the selected CIs
plot(time.grid[1:J],   hqm.est[1:J] , type="l", ylim=c(0,2), ylab="Hazard rate", xlab="time", 
      lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(tLogUpCI[1:J], rev(tLogDoCI[1:J])), 
    col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], tSymLogUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  tSymLogDoCI[1:J], lty=2, lwd=2)  
}
}
