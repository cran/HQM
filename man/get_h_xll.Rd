\name{get_h_xll}
\title{Local linear future conditional hazard rate estimator}
\alias{get_h_xll}
\description{
Calculates the (indexed) local linear future hazard rate function, conditional on a marker value \code{x}, across a set of time values \code{t}.
}

\usage{ get_h_xll(data, marker_name, br_s, event_time_name, time_name, event_name, x, b)}

\arguments{
  \item{data}{A data frame of time dependent data points.  Missing values are allowed.}
  \item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
  \item{br_s}{ User defined grid mesh on time_name variable }
  \item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
  \item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
  \item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
  \item{x}{Numeric value of the last observed marker value.}
  \item{b}{Bandwidth parameter.}
}

\details{
  The function \code{\link{get_h_xll}} uses a local linear kernel   to implement the indexed local linear future conditional hazard estimator
  \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(\hat \theta^T X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-\hat \theta^T X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x- \hat \theta^T X_i(s))\mathrm {d}s},}
across a grid of possible time values \eqn{t}, where, for a positive integer \eqn{p},  \eqn{\hat \theta^T = (\hat \theta_1, \dots, \hat \theta_p )}  is the vector of the estimated indexing parameters,  
\eqn{X_i = (X_{1, i}, \dots, X_{i,p})} is a vector of markers for indexing, \eqn{Z_i} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details. 
For \eqn{p=1} and \eqn{\hat \theta = 1} the above estimator becomes the HQM hazard rate estimator conditional on one covariate, 
 \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i( X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x- X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-  X_i(s))\mathrm {d}s},}
defined in equation (2) of Bagkavos et al. (2025) \doi{10.1093/biomet/asaf008}.  In the place of \eqn{K_b()},  \code{\link{get_h_xll}} uses the kernel    
\deqn{K_{x,b}(u)= \frac{K_b(u)-K_b(u)u^T D^{-1}c_1}{c_0 - c_1^T D^{-1} c_1}, }
where \eqn{K_b() = b^{-1}K(./b)} with  \eqn{K} being an ordinary kernel, e.g. the Epanechnikov kernel, \eqn{c_1 = (c_{11}, \dots, c_{1d})^T, D = (d_{ij})_{(d+1) \times (d+1)}} with
\deqn{
c_0   = \sum_{i=1}^n \int_0^T K_b(x-\hat \theta^T X_i(s)) Z_i(s)ds, }
\deqn{
c_{ij}  = \sum_{i=1}^n \int_0^T K_b(x-\hat \theta^T X_i(s))\{x-\hat \theta^T X_{ij}(s)\} Z_i(s)ds, }
\deqn{
d_{jk}    =  \sum_{i=1}^n \int_0^T K_b(x-\hat \theta^T X_i(s))\{x-\hat \theta^T X_{ij}(s)\}\{x-\hat \theta^T X_{ik}(s)\} Z_i(s)ds,
}
see also Nielsen (1998), \doi{10.1080/03461238.1998.10413997}.
}

\value{
  A vector of \eqn{\hat h_x(t)} for a grid of possible time values \eqn{t}.
}

 
\seealso{ \code{\link{get_alpha}, \link{h_xt},   \link{get_h_x}}}


\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}

Nielsen (1998), Marker dependent kernel hazard estimation from local linear estimation, Scandinavian Actuarial Journal,   pp.  113-124.
\doi{10.1080/03461238.1998.10413997}
}
 

\examples{
library(survival)
library(JM)

# Compare Local constant and local linear estimator for a single covariate, 
# use KM for reference.
# Albumin marker, use landmarking
Landmark <- 2
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
b=0.9

ls<-50 # 50 grid points to evaluate the estimates
s.out<- pbcT1[, 'year']
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))


arg1ll<-get_h_xll(pbcT1, 'albumin', br_s = s.out.use, event_time_name = 'years', time_name = 'year',
                  event_name = 'status2', 2, 0.9) 
arg1lc<-get_h_x(pbcT1, 'albumin', br_s = s.out.use, event_time_name = 'years', time_name = 'year',
                event_name = 'status2', 2, 0.9) 

#Calculate the local contant and local linear survival functions
br_s  = seq(Landmark, 14,  length=ls-1)
sfalb2ll<- make_sf((br_s[2]-br_s[1])/4 , arg1ll)
sfalb2lc<- make_sf((br_s[2]-br_s[1])/4 , arg1lc)

#For comparison, also calculate the Kaplan-Meier
kma2<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

#Plot the survival functions:
plot(br_s, sfalb2ll,  type="l", col=1, lwd=2, ylab="Survival probability", 
                                                        xlab="Marker level")
lines(br_s, sfalb2lc,  lty=2, lwd=2, col=2)
lines(kma2$time, kma2$surv, type="s",  lty=2, lwd=2, col=3)
legend("topright", c(  "Local linear HQM", "Local constant HQM", "Kaplan-Meier"), 
        lty=c(1, 2, 2), col=1:3, lwd=2, cex=1.7)
        

\dontrun{
#Example of get_h_xll with a single covariate (no indexing):
#Compare JM, HQM and KM for Bilirubin       
b = 10 
Landmark <- 1
lmeFit <- lme(serBilir ~ year, random = ~ year | id, data = pbc2)
coxFit <- coxph(Surv(years, status2) ~ serBilir, data = pbc2.id, x = TRUE)

jointFit0 <- jointModel(lmeFit, coxFit, timeVar = "year", 
                                method = "piecewise-PH-aGH")
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
ls<-50 # 50 grid points to evaluate the estimates
s.out<- pbcT1[, 'year']
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))
 
timesS1 <- seq(1,14,by=0.5)
predT1 <- survfitJM(jointFit0, newdata = pbcT1,survTimes = timesS1)
nm<-length(predT1$summaries)

mat.out1<-matrix(nrow=length(timesS1), ncol=nm)
for(r in 1:nm)
{
  SurvLand <- predT1$summaries[[r]][,"Mean"][1]
  mat.out1[,r] <- predT1$summaries[[r]][,"Mean"]/SurvLand
}
sfit1y<-rowMeans(mat.out1, na.rm=TRUE)

arg1<- get_h_xll(pbcT1, 'serBilir', br_s= s.out.use,  event_time_name = 'years',  
        time_name = 'year', event_name = 'status2', 1, 10) 
br_s1  = seq(Landmark, 14,  length=ls-1)
sfbil1<- make_sf((br_s1[2]-br_s1[1])/5.4 , arg1)
kma1<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

plot(br_s1, sfbil1, type="l", ylim=c(0,1), xlim=c(Landmark,14), 
                    ylab="Survival probability", xlab="years",lwd=2)
lines(timesS1, sfit1y, col=2,  lwd=2, lty=2)
lines(kma1$time, kma1$surv, type="s", lty=2, lwd=2, col=3 )
legend("bottomleft", c("HQM est.", "Joint Model est.", "Kaplan-Meier"), 
                                    lty=c(1,2,2), col=1:3, lwd=2, cex=1.7)
                                    
#Example of get_h_xll with two indexed covariates:

#First, estimate the joint model for Albumin and Bilirubin combined:
lmeFit <- lme(albumin + serBilir~ year, random = ~ year | id, data = pbc2)
coxFit <- coxph(Surv(years, status2) ~ albumin + serBilir, data = pbc2.id, 
                                                                  x = TRUE)
jointFit <- jointModel(lmeFit, coxFit, timeVar = "year", 
                                               method = "piecewise-PH-aGH")

Landmark <- 1
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
ls<-50 # 50 grid points to evaluate the estimates
s.out<- pbcT1[, 'year']
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))


# Index Albumin and Bilirubin: 
t.alb = 3  # slightly low albumin value
t.bil = 1.9  # slightly  high bilirubin value

par.alb  <- 0.0702  
par.bil <- 0.0856 
X = par.alb * pbcT1$albumin + par.bil *pbcT1$serBilir # X is now the indexed marker
t = par.alb * t.alb + par.bil *t.bil #conditioning value

pbcT1$drug<- X ## store X in place of 'drug' column which is redundant here
## i.e. 'drug' corresponds to indexed bilirubin and albumin

timesS2 <- seq(Landmark,14,by=0.5)
predT1 <- survfitJM(jointFit, newdata = pbcT1,survTimes = timesS2)
nm<-length(predT1$summaries)

mat.out1<-matrix(nrow=length(timesS2), ncol=nm)
for(r in 1:nm)
{
  SurvLand <- predT1$summaries[[r]][,"Mean"][1] #obtain mean predictions
  mat.out1[,r] <- predT1$summaries[[r]][,"Mean"]/SurvLand
}
JM.surv.est<-rowMeans(mat.out1, na.rm=TRUE) #average the resulting JM estimates

# calculate indexed local linear HQM estimator for bilirubin and albumin
b.alb = 1.5  
b.bil = 4
b.hqm   =  par.alb * b.alb + par.bil *b.bil # bandwidth for HQM estimator 
arg1<- get_h_xll(pbcT1, 'drug', br_s = s.out.use, event_time_name = 'years',  time_name = 'year', 
                                               event_name = 'status2', t, b.hqm)
br_s2  = seq(Landmark, 14,  length=ls-1) #grid points for HMQ estimator
hqm.surv.est<- make_sf((br_s2[2]-br_s2[1])/5  ,arg1) # transform HR to Survival func.

km.land<- survfit(Surv(years , status2) ~ 1, data = pbcT1) #KM estimate

#Plot the survival functions:
plot(br_s2, hqm.surv.est, type="l", ylim=c(0,1), xlim=c(Landmark,14), 
    ylab="Survival probability", xlab="years",lwd=2)
lines(timesS2, JM.surv.est, col=2,  lwd=2, lty=2)
lines(km.land$time, km.land$surv, type="s",lty=2, lwd=2, col=3)
legend("bottomleft", c("HQM est.", "Joint Model est.", "Kaplan-Meier"), 
        lty=c(1,2,2),  col=1:3, lwd=2, cex=1.7)
}

}
