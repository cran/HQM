\name{Quantile.Index.CIs}
\alias{Quantile.Index.CIs}
\title{Compute Quantile Pointwise Confidence Intervals for for the Indexed Hazard Rate Estimate}
\description{Computes quantile-bootstrap confidence intervals and its symmetric counterparts for the hazard rate, log-hazard rate, 
              and back-transformed (from log scale) hazard rate functions, based on the indexed hazard estimator.}
\usage{
  Quantile.Index.CIs(B, n.est.points, Mat.boot.haz.rate, time.grid, hqm.est, a.sig)
}
\arguments{
 \item{B}{Integer. Number of bootsrap replications.}
  \item{n.est.points}{Integer. Number of estimation points at which the indexed hazard estimates and confidence intervals are evaluated.}
  \item{Mat.boot.haz.rate}{A matrix of bootstrap estimated hazard rates with dimensions \code{n.est.points × B}, where each column corresponds to one bootstrap replicate.}
  \item{time.grid}{Numeric vector of length \code{n.est.points}: the grid points at which the indexed hazard estimates and confidence intervals are calculated.}
  \item{hqm.est}{Indexed hazard estimator, calculated at the grid points \code{time.grid} and using the original sample.}
  \item{a.sig}{The significance level (e.g., 0.05) which will be used in computing the confidence intervals.}
}


\details{
This function computes several forms of pivot confidence intervals for indexed hazard rate estimates. Set 
\eqn{ k_{\alpha/2}  = 	\hat{h}_{x}^{[\alpha/2]}(t) - \hat{h}_{x}(t)} and \eqn{ k_{1-\alpha/2} = 	\hat{h}_{x}^{[1-\alpha/2]}(t) - \hat{h}_{x}(t)} where 
  \eqn{\hat{h}_{x}^{[\alpha/2]}(t)}  is the \eqn{\alpha/2} quantile of  \eqn{\hat{h}_{x}^{(j)}(t), j=1,\dots,B}, obtained by  ordering the bootstrap estimators 
  in ascending order and selecting the \eqn{\alpha/2}-th ordered value. For example, for \eqn{B=1000} bootstrap iterations and \eqn{\alpha=0.05}, \eqn{\hat{h}_{x}^{[\alpha/2]}(t)}
  will be the 25th smallest out of the 1000 values \eqn{\hat{h}_{x}^{(j)}(t), j=1,\dots,1000}.   Also denote with \eqn{\bar k_{1-\alpha}}  the \eqn{1-\alpha} quantile of
\deqn{
  |	\hat{h}_{x}^{(j)}(t) - \hat{h}_{x}(t)|, j=1,\dots, B.
}
 Then, the quantile bootstrap CI for   \eqn{\hat{h}_{x}(t)} is given by
\deqn{
     \Bigg (  \hat{h}_{x}(t) -  k_{1-\alpha/2},     \hat{h}_{x}(t) -  k_{\alpha/2}  \Bigg ).  
}
The symmetric quantile CI (basic CI) is
\deqn{
\Bigg (  \hat{h}_{x}(t) -  \bar k_{1-\alpha},     \hat{h}_{x}(t) +  \bar k_{1-\alpha}  \Bigg ).   
}
    
The confidence intervals for the logarithm of the hazard rate function are defined as follows. First set 
\eqn{
k_{\alpha/2}^L    = 	\hat{L}_{x}^{[\alpha/2]}(t) - \hat{L}_{x}(t)} and  \eqn{k_{1-\alpha/2}^L   = 	\hat{L}_{x}^{[1-\alpha/2]}(t) - \hat{L}_{x}(t)}.
 
Also denote with \eqn{\bar k_{1-\alpha}^L}   the \eqn{1-\alpha} quantile of \eqn{  |	\hat{L}_{x}^{(j)}(t) - \hat{L}_{x}(t)|, j=1,\dots, B. }
For the log hazard function \eqn{L_x(t)} we have the quantile confidence interval is
\deqn{
     \Bigg (  \hat{L}_{x}(t) -  k_{1-\alpha/2}^L,     \hat{L}_{x}(t) -  k_{\alpha/2}^L  \Bigg ).  
}
The corresponding symmetric quantile CI for the log hazard is
\deqn{
\Bigg (  \hat{L}_{x}(t) -  \bar k_{1-\alpha}^L,     \hat{L}_{x}(t) +  \bar k_{1-\alpha}^L  \Bigg ).   
}
These confidence intervals are  transformed back to confidence intervals for the hazard rate function \eqn{h_x(t)}:
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-    k_{1-\alpha/2}^L},     \hat{h}_{x}(t) e^{-    k_{\alpha/2}^L}  \Bigg ).   
}
The corresponding symmetric confidence interval is
\deqn{
\Bigg (  \hat{h}_{x}(t) e^{-   \bar k_{1-\alpha}^L},     \hat{h}_{x}(t) e^{ \bar k_{1-\alpha}^L}  \Bigg ).  
}  

Note: The bootstrap matrix \code{Mat.boot.haz.rate} is assumed to contain estimates  produced using the same time grid as \code{time.grid} and the same estimator used  to generate \code{hqm.est}.
}
\value{
  A data frame with the following columns:
    
  \item{time}{The evaluation grid points.}
  \item{est}{Indexed hazard rate estimator \code{hqm.est}.}
  \item{downci, upci}{Lower and upper endpoints of basic quantile CIs.}
  \item{docisym, upcisym}{Lower and upper endpoints of symmetric quantile CIs.}
  \item{logdoci, logupci}{Lower and upper endpoints of quantile CIs on the log-scale.}
  \item{logdocisym, logupcisym}{Symmetric log-scale CIs.}
  \item{log.est}{The logarithm of the indexed hazard rate estimate, \code{log(hqm.est)}.}
  \item{tLogDoCI, tLogUpCI}{Transformed-log CIs based on \code{2*log(hqm.est) - log-quantiles}.}
  \item{tSymLogDoCI, tSymLogUpCI}{Symmetric transformed-log CIs.}
}
\seealso{
  \code{\link{Boot.hrandindex.param}},   \code{\link{Boot.hqm}}
}

%\author{ Your Name }

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
b = 0.42# The result, on the indexed marker 'indmar' of 
         #\code{b_selection(pbc2, 'indmar', 'years', 'year', 'status2', I=26, seq(0.2,0.4,by=0.01))} 
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

#  Create bootstrap samples by group: 
set.seed(1)  
B<- 50   #for display purposes only; for sensible results use B=1000 (slower) 
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
              event_name = event_name, in.par = c(par.x1, par.x2), b)

# Bootstrap the original indexed HQM estimator: 
res <- Boot.hrandindex.param(B, Boot.samples, marker_name1, marker_name2, event_time_name,
                            time_name,  event_name, b = 0.4, t = t, true.haz = true.hazard, 
                            v.param = c(0.07, 0.08), n.est.points   )
J <- 80
a.sig<-0.05   

# Construct Ci's: 
all.cis.quant<- Quantile.Index.CIs(B, n.est.points, res, time.grid, hqm.est, a.sig)

# extract Plain   + symmetric CIs and plot them:
UpCI<-all.cis.quant[,"upci"]
DoCI<-all.cis.quant[,"downci"]
SymUpCI<-all.cis.quant[,"upcisym"]
SymDoCI<-all.cis.quant[,"docisym"]

#Plot the selected CIs
plot(time.grid[1:J],   hqm.est[1:J], type="l", ylim=c(0,2), ylab="Hazard rate", 
      xlab="time", lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(UpCI[1:J], rev(DoCI[1:J])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  SymDoCI[1:J], lty=2, lwd=2)   

# extract transformed from Log HR + symmetric CIs 
LogUpCI<-all.cis.quant[,"logupci"]
LogDoCI<-all.cis.quant[,"logdoci"]
SymLogUpCI<-all.cis.quant[,"logupcisym"]
SymLogDoCI<-all.cis.quant[,"logdocisym"]

#Plot the selected CIs
plot(time.grid[1:J],   hqm.est[1:J], type="l", ylim=c(0,2), ylab="Hazard rate", 
      xlab="time", lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(LogUpCI[1:J], rev(LogDoCI[1:J])), 
        col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], SymLogUpCI[1:J], lty=2, lwd=2 )#, lwd=3
lines(time.grid[1:J],  SymLogDoCI[1:J], lty=2, lwd=2)  

# extract Log HR + symmetric CIs 
tLogUpCI<-all.cis.quant[,"tLogUpCI"]
tLogDoCI<-all.cis.quant[,"tLogDoCI"]
tSymLogUpCI<-all.cis.quant[,"tSymLogUpCI"]
tSymLogDoCI<-all.cis.quant[,"tSymLogDoCI"]

#Plot the selected CIs
plot(time.grid[1:J],  log(hqm.est[1:J]), type="l", ylim=c(-5,4), ylab="Log Hazard rate", 
     xlab="time", lwd=2)
polygon(x = c(time.grid[1:J], rev(time.grid[1:J])), y = c(tLogUpCI[1:J], rev(tLogDoCI[1:J])), 
      col =  adjustcolor("red", alpha.f = 0.50), border = NA )
lines(time.grid[1:J], tSymLogUpCI[1:J], lty=2, lwd=2 ) 
lines(time.grid[1:J],  tSymLogDoCI[1:J], lty=2, lwd=2)  
}   
}
