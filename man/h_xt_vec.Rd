\name{h_xt_vec}
\title{Hqm estimator on the marker grid}
\alias{h_xt_vec}
\description{
Computes the (indexed) hqm estimator on the marker grid.
}

\usage{
h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
}

\arguments{
\item{br_X}{Marker value grid points that will be used in the evaluatiuon.}
\item{br_s}{Time value grid points that will be used in the evaluatiuon.}
\item{size_s_grid}{Size of the time grid.}
\item{alpha}{Marker-hazard obtained from \code{\link{get_alpha}}.}
\item{t}{Numeric value of the time the function should be evaluated.}
\item{b}{Bandwidth.}
\item{Yi}{A matrix made by \code{\link{make_Yi}} indicating the exposure. }
\item{int_X}{Position of the linear interpolated marker values on the marker grid.}
\item{n}{Number of individuals.}
}

\details{
The function implements the future conditional hazard estimator
\deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i(\hat \theta^T X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x-\hat \theta^T X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x- \hat \theta^T X_i(s))\mathrm {d}s},}
for every \eqn{x} on the marker grid,  where, for a positive integer \eqn{p},  \eqn{\hat \theta^T = (\hat \theta_1, \dots, \hat \theta_p )}  is the vector of the estimated indexing parameters,  
\eqn{X_i = (X_{1, i}, \dots, X_{i,p})} is a vector of markers for indexing, \eqn{Z_i} is the exposure and \eqn{\alpha(z)} is the marker-only hazard, see \code{\link{get_alpha}} for more details 
and \eqn{K_b = b^{-1}K(./b)} is an ordinary kernel function, e.g. the Epanechnikov kernel.
For \eqn{p=1} and \eqn{\hat \theta = 1} the above estimator becomes the HQM hazard rate estimator conditional on one covariate, 
 \deqn{\hat{h}_x(t) = \frac{\sum_{i=1}^n \int_0^T\hat{\alpha}_i( X_i(t+s))Z_i(t+s)Z_i(s)K_{b}(x- X_i(s))\mathrm {d}s}{\sum_{i=1}^n\int_0^TZ_i(t+s)Z_i(s)K_{b}(x-  X_i(s))\mathrm {d}s},}
defined in equation (2) of Bagkavos et al. (2025), \doi{10.1093/biomet/asaf008}. 
}


 

\value{
A vector of \eqn{\hat{h}_{x}(t)} for all values \eqn{x} on the marker grid.
}



 
\references{
Bagkavos, I., Isakson, R., Mammen, E., Nielsen, J., and Proust–Lima, C. (2025). 
Biometrika, 112(2), asaf008. \doi{10.1093/biomet/asaf008}
}
 
 
\examples{

# Longitudinal data example

pbc2_id = to_id(pbc2)
size_s_grid <- size_X_grid <- 100
n = max(as.numeric(pbc2$id))
s = pbc2$year
X = pbc2$serBilir
XX = pbc2_id$serBilir
ss <- pbc2_id$years
delta <- pbc2_id$status2
br_s = seq(0, max(s), max(s)/( size_s_grid-1))
br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)

int_X <- findInterval(X_lin, br_X)
int_s = rep(1:length(br_s), n)

N <- make_N(pbc2, pbc2_id, br_X, br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s,
            size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)

b = 1.7
alpha<-get_alpha(N, Y, b, br_X, K=Epan )

Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s,
              size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n)

t = 2

h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)

# Time-invariant data example:
# pbc2 dataset, single event per individual version:
# 312 observations, most recent event per individual.
# Use landmarking to produce comparable curve with KM.
library(survival)
Landmark <- 3 #set the landmark to 3 years
pbc2.use<- to_id(pbc2) # keep only the most recent row per patient
pbcT1 <- pbc2.use[which(pbc2.use$year< Landmark  & pbc2.use$years> Landmark),]

timesS2 <- seq(Landmark,14,by=0.5)
b=0.9
arg1<- get_h_x(pbcT1, 'albumin', br_s=br_s,  event_time_name = 'years',  time_name = 'year', 
                                                    event_name = 'status2', 2, b) 
br_s2  = seq(Landmark, 14,  length=99)
sfalb2<- make_sf(    (br_s2[2]-br_s2[1])/1.35 , arg1)
kma2<- survfit(Surv(years , status2) ~ 1, data = pbcT1)

#Plot the survival functions:
plot(br_s2, sfalb2, type="l", ylim=c(0,1), xlim=c(Landmark,14), ylab="Survival probability", 
            xlab="years",lwd=2, main="HQM and KM survival functions, conditional on albumin=2, 
            for the time-invariant pbc dataset")
lines(kma2$time, kma2$surv, type="s",lty=2, lwd=2, col=2)
legend("bottomleft", c("HQM est.", "Kaplan-Meier"), lty=c(1,2), col=1:2, lwd=2, cex=1.7)


############# Indexed HR estimator example 1: ##############
### The example is based on indexing two covariates: albumin and bilirubin

index.use<-84
x.grid<-br_s[1:index.use] #non need to plot the last year

b.alb = 1.5 # results from CV rule of Bagkavos et al. (2025), i.e.: 
 # b_list = seq(1.5, 3, 0.05)
 # b_scores_alb = b_selection(pbc2, 'albumin', 'years', 'year', 'status2', I, b_list)
 # b.alb =  b_scores_alb[[2]][which.min(b_scores_alb[[1]])]
 
b.bil = 4 # results from CV rule of Bagkavos et al. (2025) : 
  # b_list.bil = seq(3, 4, 0.05)
  # b_scores_bil = b_selection(pbc2, 'serBilir', 'years', 'year', 'status2', I, b_list.bil)
  # b.bil =  b_scores_alb[[2]][which.min(b_scores_alb[[1]])] - result = 4

t.alb = 1 # refers to zero mean variables  
          #corresponds to approximately normal albumin level
t.bil = 1.9 # refers to zero mean variable - corresponds to 
          #elevated bilirubin levelm approximately 7mg/dl

par.alb  <- 0.0702 # results from the indexing process
par.bil <- 0.0856 # results from the indexing process

# First, mean adjust the albumin covariate
Xalb = pbc2$albumin - mean(pbc2$albumin)
XXalb = pbc2_id$albumin - mean(pbc2_id$albumin)

br_Xalb = seq(min(Xalb), max(Xalb), (max(Xalb)-min(Xalb))/( size_X_grid-1))
X_linalb = lin_interpolate(br_s, pbc2_id$id, pbc2$id, Xalb, s)
int_Xalb <- findInterval(X_linalb, br_Xalb)
N.alb <- make_N(pbc2, pbc2_id, br_Xalb, br_s, ss, XXalb, delta)
Y.alb <- make_Y(pbc2, pbc2_id, X_linalb, br_Xalb, br_s,
            size_s_grid, size_X_grid, int_s, int_Xalb, event_time = 'years', n)

alpha.alb<-get_alpha(N.alb, Y.alb, b.alb, br_Xalb, K=Epan )
Yi.alb <- make_Yi(pbc2, pbc2_id, X_linalb, br_Xalb, br_s, size_s_grid, size_X_grid, 
                                          int_s, int_Xalb, event_time = 'years', n)

#calculate HQM estimator for original albumin covariate:
arg.alb<- h_xt_vec(br_Xalb, br_s, size_s_grid, alpha.alb, t.alb, b.alb, Yi.alb, 
                                                                  int_Xalb, n)

y.grid.alb<-arg.alb[1:index.use]

# Now, mean adjust the bilirubon covariate:
Xbil = pbc2$serBilir - mean(pbc2$serBilir)
XXbil = pbc2_id$serBilir - mean(pbc2_id$serBilir)

br_Xbil = seq(min(Xbil), max(Xbil), (max(Xbil)-min(Xbil))/( size_X_grid-1))
X_linbil = lin_interpolate(br_s, pbc2_id$id, pbc2$id, Xbil, s)
int_Xbil <- findInterval(X_linbil, br_Xbil)
N.bil <- make_N(pbc2, pbc2_id, br_Xbil, br_s, ss, XXbil, delta)
Y.bil <- make_Y(pbc2, pbc2_id, X_linbil, br_Xbil, br_s,
            size_s_grid, size_X_grid, int_s, int_Xbil, event_time = 'years', n)

alpha.bil<-get_alpha(N.bil, Y.bil, b.bil, br_Xbil, K=Epan )
Yibil <- make_Yi(pbc2, pbc2_id, X_linbil, br_Xbil, br_s, size_s_grid, size_X_grid, 
                                        int_s, int_Xbil, event_time = 'years', n)

#calculate HQM estimator for original bilirubin covariate:
arg1.bil<- h_xt_vec(br_Xbil, br_s, size_s_grid, alpha.bil, t.bil, b.bil, Yibil, 
                                                                    int_Xbil, n)
y.grid.bil<-arg1.bil[1:index.use]

# calculate the indexed variables:
X = par.alb * Xalb + 	par.bil *Xbil
XX = par.alb * XXalb + par.bil *XXbil
b = 0.4 # results from CV rule in Bagkavos et al (2025) 
t = par.alb * t.alb + par.bil *t.bil

br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)
int_X <- findInterval(X_lin, br_X)
N <- make_N(pbc2, pbc2_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, 
                                                             int_X, 'years', n)
alpha<-get_alpha(N, Y, b, br_X, K=Epan )
Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, 
                                                      int_s, int_X,'years', n)

# Now calculate indexed HR estimator:
arg2<- h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n) 
y.grid2<-arg2[1:index.use] 
 

############## Plot the results:
plot(x.grid, y.grid2, type="l", ylim=c(0,2), xlab="Years", ylab="Hazard rate", lwd=2 )
lines(x.grid, y.grid.bil , lty=2, col=4, lwd=2)
lines(x.grid, y.grid.alb, lty=2, col=2, lwd=2)

legend("topleft", lty=c(1, 2, 2), lwd=c(2,2,2),
       c("Indexed (Bilirubin and Albumin) HQM hazard rate est.", 
         "HQM hazard rate est. conditional on Albumin=2mg/dl", 
         "HQM hazard rate est. conditional on Bilirubin=7mg/dl" ),
       col=c(1,2,4), cex=2, bty="n")

############# Indexed HR estimator example 2: ##############
### The example is based on indexing two covariates:  bilirubin and prothrombin time

index.use<-84
x.grid<-br_s[1:index.use]

b.pro = 3
b.bil = 4

t.pro = 1.5 # refers to zero mean variables - slightly high
t.bil = 1.9 # refers to zero mean variable - high

par.pro  <- 0.349 #0.5 #0.149
par.bil <- 0.0776 #0.10

############ prothrombin time #############
ind1 <- which( is.na( pbc2$prothrombin ) )
Xpro1 =   pbc2$prothrombin   
Xpro1[ind1]<-mean(pbc2$prothrombin, na.rm=TRUE)
Xpro1 <- Xpro1 - mean(Xpro1)

ind2 <- which( is.na( pbc2_id$prothrombin ) )
XXpro1 =   pbc2_id$prothrombin   
XXpro1[ind2]<-mean(pbc2_id$prothrombin, na.rm=TRUE)
XXpro1 <- XXpro1 - mean(XXpro1)

br_Xbil = seq(min(Xpro1), max(Xpro1), (max(Xpro1)-min(Xpro1))/( size_X_grid-1))
X_linbil = lin_interpolate(br_s, pbc2_id$id, pbc2$id, Xpro1, s)
int_Xbil <- findInterval(X_linbil, br_Xbil)
N.bil <- make_N(pbc2, pbc2_id, br_Xbil, br_s, ss, XXpro1, delta)
Y.bil <- make_Y(pbc2, pbc2_id, X_linbil, br_Xbil, br_s,
                size_s_grid, size_X_grid, int_s, int_Xbil, event_time = 'years', n)

alpha.bil<-get_alpha(N.bil, Y.bil, b.pro, br_Xbil, K=Epan )
Yibil <- make_Yi(pbc2, pbc2_id, X_linbil, br_Xbil, br_s, size_s_grid, size_X_grid, int_s, 
                                                          int_Xbil, event_time = 'years', n)
arg1.pro<- h_xt_vec(br_Xbil, br_s, size_s_grid, alpha.bil, t.pro, b.pro, Yibil, int_Xbil, n)
y.grid.pro<-arg1.pro[1:index.use]


############ Bilirubin #############
Xbil = pbc2$serBilir - mean(pbc2$serBilir)
XXbil = pbc2_id$serBilir - mean(pbc2_id$serBilir)

br_Xbil = seq(min(Xbil), max(Xbil), (max(Xbil)-min(Xbil))/( size_X_grid-1))
X_linbil = lin_interpolate(br_s, pbc2_id$id, pbc2$id, Xbil, s)
int_Xbil <- findInterval(X_linbil, br_Xbil)
N.bil <- make_N(pbc2, pbc2_id, br_Xbil, br_s, ss, XXbil, delta)
Y.bil <- make_Y(pbc2, pbc2_id, X_linbil, br_Xbil, br_s,
            size_s_grid, size_X_grid, int_s, int_Xbil, event_time = 'years', n)

alpha.bil<-get_alpha(N.bil, Y.bil, b.bil, br_Xbil, K=Epan )
Yibil <- make_Yi(pbc2, pbc2_id, X_linbil, br_Xbil, br_s, size_s_grid, size_X_grid, int_s, 
                                                          int_Xbil, event_time = 'years', n)
arg1.bil<- h_xt_vec(br_Xbil, br_s, size_s_grid, alpha.bil, t.bil, b.bil, Yibil, int_Xbil, n)
y.grid.bil<-arg1.bil[1:index.use]

######### Index prothrombin time and bilirubin
X = par.pro * Xpro1 + 	par.bil *Xbil
XX = par.pro * XXpro1 + par.bil *XXbil

b = 1.5  
t = par.pro * t.pro + par.bil *t.bil

br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

X_lin = lin_interpolate(br_s, pbc2_id$id, pbc2$id, X, s)
int_X <- findInterval(X_lin, br_X)
N <- make_N(pbc2, pbc2_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
Y <- make_Y(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X, 
                                                                          'years', n)
alpha<-get_alpha(N, Y, b, br_X, K=Epan )
Yi <- make_Yi(pbc2, pbc2_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X,
                                                                          'years', n)
arg2<- h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
y.grid2<-arg2[1:index.use]
 


##############Plot the results
plot(x.grid, y.grid2, type="l", ylim=c(0,2), xlab="Years", ylab="Hazard rate", lwd=2 )
lines(x.grid, y.grid.bil , lty=2, col=4, lwd=2)

lines(x.grid, y.grid.pro, lty=2, col=2, lwd=2)
legend("topleft", lty=c(1, 2, 2), lwd=c(2,2,2),
       c("Indexed (Bilirubin and PT) HQM hazard rate est.", 
        "HQM hazard rate est. conditional on PT=15.5s", 
        "HQM hazard rate est. conditional on Bilirubin=7mg/dl" ),
        col=c(1,2,4), cex=2, bty="n")

}
