\name{bs.hqm}
\title{Brier score for the High Quality Marker estimator}
\alias{bs.hqm}
\description{
Calculates the Brier score for the HQM estimator.
}

\usage{bs.hqm(xin, est, landm, th, event_time_name, status_name)}

\arguments{
\item{xin}{A data frame containing event times and the patient status.}
\item{est}{The HQM estimator values, typically the output of \code{\link{get_h_x}}. }
\item{landm}{Landmark time.}
\item{th}{Time horizon.}
\item{event_time_name}{The column name of the event times in the data frame \code{xin}. }
\item{status_name}{The column name of the status variable in the data frame \code{xin}.}
}

\details{
The function \code{\link{bs.hqm}} implements the Brier score calculation for the HQM estimator estimator.
}

\value{
Scalar: the Brier score of the HQM estimator.
}




\seealso{ \code{\link{auc.hqm} }}

\examples{
library(pec)
library(survival)
Landmark <- 2

pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
ls<-50 # 50 grid points to evaluate the estimates
s.out<- pbcT1[, 'year']
s.out.use <-  seq(0, max(s.out), max(s.out)/( ls-1))
 
timesS2 <- seq(Landmark,14,by=0.5)


b=0.9
arg1<- get_h_x(pbcT1, 'albumin', br_s = s.out.use, event_time_name = 'years',  
               time_name = 'year', event_name = 'status2', 2, 0.9) 
br_s2  = seq(Landmark, 14,  length=ls-1)
sfalb2<- make_sf(    (br_s2[2]-br_s2[1])/4 , arg1)


tHor <- 1.5
bs.use<-bs.hqm(pbcT1, sfalb2, Landmark,tHor, 
                event_time_name = 'years', status_name = 'status2')
bs.use
}
