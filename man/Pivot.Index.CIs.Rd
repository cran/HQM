\name{Pivot.Index.CIs}
\alias{Pivot.Index.CIs}
\title{Compute Pivot Pointwise Confidence Intervals for the Indexed Hazard Rate Estimate}
\description{Computes bootstrap pivot and symmetric bootstrap-pivot confidence intervals for the hazard, log-hazard, and 
          back-transformed (log-scale) hazard rate functions, using the indexed hazard estimator.}
\usage{
  Pivot.Index.CIs(B,  n.est.points, Mat.boot.haz.rate, time.grid, hqm.est, a.sig)
}
\arguments{
 \item{B}{Integer. Number of bootsrap replications }
  \item{n.est.points}{Integer. Number of estimation points at which the indexed hazard estimates and confidence intervals are evaluated.}
  \item{Mat.boot.haz.rate}{A matrix of bootstrap estimated hazard rates with 
    dimensions \code{n.est.points × B}, where each column corresponds to one bootstrap replicate.}
  \item{time.grid}{Numeric vector of length \code{n.est.points}: the grid points at which the indexed hazard estimates and confidence intervals are calculated.}
  \item{hqm.est}{Indexed hazard estimator, calculated at the grid points \code{time.grid} and using the original sample.}
  \item{a.sig}{The significance level (e.g., 0.05) which will be used in computing the confidence intervals.}
}
\details{
This function computes several forms of pivot confidence intervals for indexed hazard rate estimates. Let 
\eqn{\hat \sigma} be the sample standard deviation of the bootstrap estimators 	\eqn{\hat{h}_{x}^{(j)}(t), j=1,\dots, B} 
and let \eqn{k_{\alpha/2}^p, k_{1-\alpha/2}^p} and \eqn{\bar k_{1-\alpha}^p} be the  \eqn{\alpha/2, 1-\alpha/2} and \eqn{1-\alpha} 
quantiles respectively  of
\deqn{
\frac{\hat{h}_{x}^{(j)}(t) - \hat{h}_{x}(t)}{\hat \sigma},\; j=1,\dots, B.
}
Then, the pivot bootstrap confidence interval (CI) for \eqn{\hat{h}_{x}(t)} is given by
\deqn{
     \Bigg (  \hat{h}_{x}(t) -  \hat \sigma k_{1-\alpha/2}^p,     \hat{h}_{x}(t) -  \hat \sigma k_{\alpha/2}^p  \Bigg ).  
}
The symmetric pivot bootstrap  CI for   \eqn{\hat{h}_{x}(t)} is
\deqn{
\Bigg (  \hat{h}_{x}(t) - \hat \sigma \bar k_{1-\alpha}^p,     \hat{h}_{x}(t) +  \hat \sigma\bar k_{1-\alpha}^p  \Bigg ).   
}
For the confidence intervals for the logarithm of the hazard rate function, first let 
\deqn{
L_x(t)    = 	\log \left \{ h_x(t)\right \},  
\hat L_{x}(t)   = 	\log \left \{ \hat h_{x}(t)\right \},  
\hat L^{(j)}_{x}(t)   = 	\log \left \{ \hat h_{x}^{(j)}(t)\right \},
}
and \eqn{	k_{\alpha/2}^{L,p},	k_{1-\alpha/2}^{L,p}} and \eqn{\bar k_{1-\alpha}^{L,p}}  be respectively the \eqn{\alpha/2, 1-\alpha/2} and \eqn{1-\alpha} quantile of 
\deqn{\frac{|\hat{L}_{x}^{(j)}(t) - \hat{L}_{x}(t)|}{\hat \sigma}, j=1,\dots, B.
}
For the log hazard function \eqn{L_x(t)} a pivotal bootstrap confidence interval is
 
\deqn{
     \Bigg (  \hat{L}_{x}(t) -  \hat \sigma k_{1-\alpha/2}^{L,p},     \hat{L}_{x}(t) -  \hat \sigma k_{\alpha/2}^{L,p}  \Bigg ).   
}
A symmetric pivotal bootstrap  CI for the log hazard is
\deqn{
\Bigg (  \hat{L}_{x}(t) -  \hat \sigma \bar k_{1-\alpha}^{L,p},     \hat{L}_{x}(t) +  \hat \sigma \bar k_{1-\alpha}^{L,p}  \Bigg ).    
}
These last two confidence intervals   can be transformed back to yield confidence intervals for the hazard rate function \eqn{h_x(t)}. 
These are:
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-   \hat \sigma k_{1-\alpha/2}^{L,p}},   \hat{h}_{x}(t) e^{- \hat \sigma   k_{\alpha/2}^{L,p}}  \Bigg ),   
}
and  
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-  \hat \sigma \bar k_{1-\alpha}^{L,p}},     \hat{h}_{x}(t) e^{ \hat \sigma  \bar k_{1-\alpha}^{L,p}}  \Bigg ) 
}
respectively. 
  
Note: The bootstrap matrix \code{Mat.boot.haz.rate} is assumed to contain estimates  produced using the same time grid as \code{time.grid} and the same estimator used  to generate \code{hqm.est}.
}

\value{A data frame with the following columns:
    
  \item{time}{The evaluation grid points.}
  \item{est}{Indexed hazard rate estimator \code{hqm.est}.}
  \item{downci, upci}{Lower and upper endpoints of basic pivot CIs.}
  \item{docisym, upcisym}{Lower and upper endpoints of symmetric pivot CIs.}
  \item{logdoci, logupci}{Lower and upper endpoints of pivot CIs on the log-scale.}
  \item{logdocisym, logupcisym}{Symmetric pivot CIs on the log-scale.}
  \item{log.est}{The logarithm of the indexed hazard rate estimate, \code{log(hqm.est)}.}
  \item{tLogDoCI, tLogUpCI}{Transformed-log CIs based on   \code{2*log(hqm.est) - log-quantiles}.}
  \item{tSymLogDoCI, tSymLogUpCI}{Symmetric transformed-log CIs.}
  
}
\seealso{
  \code{\link{Boot.hrandindex.param}},  
  \code{\link{Boot.hqm}}
}

%\author{
%  Your Name
%}

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
b = 0.42#par.alb * b.alb + par.bil *b.bil # 7
t = par.x1 * t.x1 + par.x2 *t.x2
ls<-50

#Store original sample values:
xin <- pbc2[,c(id, marker_name1, marker_name2, event_time_name, time_name, event_name)]
n <- length(xin$id)
nn<-max(  as.double(xin[,'id']) )

xin.id <- to_id(xin)

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
B<- 50  #for display purposes only; for sensible results use B=1000 (slower) 
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
res <- Boot.hrandindex.param(B, Boot.samples, marker_name1, marker_name2, 
                  event_time_name, time_name,  event_name, b = 0.4, t = t, 
                  true.haz = true.hazard, v.param = c(0.07, 0.08), n.est.points)

# Construct Ci's: 
a.sig<-0.05   
all.cis.pivot<- Pivot.Index.CIs(B, n.est.points, res, time.grid, hqm.est, a.sig)

# extract Plain   + symmetric CIs 
UpCI<-all.cis.pivot[,"UpCI"]
DoCI<-all.cis.pivot[,"DoCI"]
SymUpCI<-all.cis.pivot[,"SymUpCI"]
SymDoCI<-all.cis.pivot[,"SymDoCI"]

J <- 80 #select the first 80 grid points (for display purposes only)
#Plot the selected CIs
plot(time.grid[1:J],   hqm.est[1:J], type="l", ylim=c(0,2), ylab="Hazard rate", 
      xlab="time", lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(UpCI[1:J], rev(DoCI[1:J])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  SymDoCI[1:J], lty=2, lwd=2)   

# extract transformed from Log HR + symmetric CIs 
LogUpCI<-all.cis.pivot[,"LogUpCI"]
LogDoCI<-all.cis.pivot[,"LogDoCI"]
SymLogUpCI<-all.cis.pivot[,"LogSymUpCI"]
SymLogDoCI<-all.cis.pivot[,"LogSymDoCI"]

#Plot the selected CIs
plot(time.grid[1:J],   hqm.est[1:J] , type="l", ylim=c(0,2), ylab="Log Hazard rate", 
      xlab="time", lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(LogUpCI[1:J], rev(LogDoCI[1:J])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymLogUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  SymLogDoCI[1:J], lty=2, lwd=2)  

# extract Log HR + symmetric CIs  
tLogUpCI<-all.cis.pivot[,"LogtUpCI"]
tLogDoCI<-all.cis.pivot[,"LogTDoCI"]
tSymLogUpCI<-all.cis.pivot[,"SymLogtUpCI"]
tSymLogDoCI<-all.cis.pivot[,"SymLogTDoCI"]

#Plot the selected CIs
plot(time.grid[1:J], log(hqm.est[1:J] ), type="l", ylim=c(-5,4), ylab="Hazard rate", 
      xlab="time",  lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(tLogUpCI[1:J], rev(tLogDoCI[1:J])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], tSymLogUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  tSymLogDoCI[1:J], lty=2, lwd=2)  
}
}
