\name{get_h_x}
\title{Local constant future conditional hazard rate estimator}
\alias{get_h_x}
\description{
Calculates the (indexed)  local constant future hazard rate function, conditional on a marker value \code{x}, across across a set of time values \code{t}.
}

\usage{get_h_x(data, marker_name, event_time_name, time_name, event_name, x, b)}

\arguments{
\item{data}{A data frame of time dependent data points.  Missing values are allowed.}
\item{marker_name}{The column name of the marker values in the data frame \code{\link{data}}. }
\item{event_time_name}{The column name of the event times in the data frame \code{\link{data}}. }
\item{time_name}{The column name of the times the marker values were observed in the data frame \code{\link{data}}.}
\item{event_name}{The column name of the events in the data frame \code{\link{data}}.}
\item{x}{Numeric value of the last observed marker value.}
\item{b}{Bandwidth parameter.}
}

\details{
  The function \code{\link{get_h_x}} implements the indexed local linear future conditional hazard estimator
  \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(\hat \theta^T X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-\hat \theta^T X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x- \hat \theta^T X_i(s))\mathrm {d}s},}
across a grid of possible time values \eqn{t}, where, for a positive integer \eqn{p},  \eqn{\hat \theta^T = (\hat \theta_1, \dots, \hat \theta_p )}  is the vector of the estimated indexing parameters,  
\eqn{X_i = (X_{1, i}, \dots, X_{i,p})} is a vector of markers for indexing, \eqn{Z_i} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details 
and \eqn{K_b = b^{-1}K(./b)} is an ordinary kernel function, e.g. the Epanechnikov kernel.
For \eqn{p=1} and \eqn{\hat \theta = 1} the above estimator becomes the HQM hazard rate estimator conditional on one covariate, 
 \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i( X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x- X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-  X_i(s))\mathrm {d}s},}
defined in equation (2) of \doi{10.1093/biomet/asaf008}. 
}

\value{
A vector of \eqn{\hat h_x(t)} for a grid of possible time values \eqn{t}.
}




\seealso{ \code{\link{get_alpha}, \link{h_xt},   \link{get_h_xll} }}

\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}
}


\examples{
library(survival)
b = 10
x = 3
Landmark <- 2
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
b=0.9

arg1ll<-get_h_xll(pbcT1,'albumin',event_time_name='years',
                  time_name='year',event_name='status2',2,0.9) 
arg1lc<-get_h_x(pbcT1,'albumin',event_time_name='years',
                time_name='year',event_name='status2',2,0.9) 

#Caclulate the local contant and local linear survival functions
br_s  = seq(Landmark, 14,  length=99)
sfalb2ll<- make_sf(    (br_s[2]-br_s[1])/4 , arg1ll)
sfalb2lc<- make_sf(    (br_s[2]-br_s[1])/4 , arg1lc)

#For comparison, also calculate the Kaplan-Meier
kma2<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

#Plot the survival functions:
plot(br_s, sfalb2ll,  type="l", col=1, lwd=2, ylab="Survival probability", 
     xlab="Marker level")
lines(br_s, sfalb2lc,  lty=2, lwd=2, col=2)
lines(kma2$time, kma2$surv, type="s",  lty=2, lwd=2, col=3)

legend("topright", c(  "Local linear HQM", "Local constant HQM", 
        "Kaplan-Meier"), lty=c(1, 2, 2), col=1:3, lwd=2, cex=1.7)
        
        
\dontrun{
#Example of get_h_x with two indexed covariates:
#First, estimate the joint model for Albumin and Bilirubin combined:
library(JM)
lmeFit <- lme(albumin + serBilir~ year, random = ~ year | id, data = pbc2)
coxFit <- coxph(Surv(years, status2) ~ albumin + serBilir, data = pbc2.id, x = TRUE)
jointFit <- jointModel(lmeFit, coxFit, timeVar = "year", method = "piecewise-PH-aGH")

Landmark <- 1
pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]

# Index Albumin and Bilirubin: 
t.alb = 3  # slightly low albumin value
t.bil = 1.9  # slightly  high bilirubin value

par.alb  <- 0.0702  
par.bil <- 0.0856 
X = par.alb * pbcT1$albumin + par.bil *pbcT1$serBilir # X is now the indexed marker
t = par.alb * t.alb + par.bil *t.bil #conditioning value

pbcT1$drug<- X ## store the combine variable in place of 'drug' column which is redundant
## i.e. 'drug' corresponds to indexed bilirubin and albumin

timesS2 <- seq(Landmark,14,by=0.5)
predT1 <- survfitJM(jointFit, newdata = pbcT1, survTimes = timesS2)
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
arg1<- get_h_x(pbcT1, 'drug', event_time_name = 'years',  time_name = 'year', 
                 event_name = 'status2', t, b.hqm)
br_s2  = seq(Landmark, 14,  length=99) #grid points for HMQ estimator
hqm.surv.est<- make_sf((br_s2[2]-br_s2[1])/5, arg1) # transform HR to Survival func.

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