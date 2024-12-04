\name{auc.hqm}
\title{AUC for the High Quality Marker estimator}
\alias{auc.hqm}
\description{
Calculates the AUC for the HQM estimator.
}

\usage{auc.hqm(xin, est, landm, th, event_time_name, status_name)}

\arguments{
\item{xin}{A data frame containing event times and the patient status.}
\item{est}{The HQM estimator values, typically the output of \code{\link{get_h_x}}. }
\item{landm}{Landmark time.}
\item{th}{Time horizon.}
\item{event_time_name}{The column name of the event times in the \code{xin} data frame. }
\item{status_name}{The column name of the status variable in the \code{xin}  frame.}
}

\details{
The function \code{\link{auc.hqm}} implements the AUC calculation for the HQM estimator estimator.
}

\value{
A vector of two values: the landmark time of the calculation and the AUC value.
}




\seealso{ \code{\link{bs.hqm} }}

\examples{
library(timeROC)
library(survival)
 Landmark <- 2
 pbcT1 <- pbc2[which(pbc2$year< Landmark  & pbc2$years> Landmark),]
 timesS2 <- seq(Landmark,14,by=0.5)
 b=0.9
 arg1<- get_h_x(pbcT1, 'albumin', event_time_name = 'years',  
                time_name = 'year', event_name = 'status2', 2, 0.9) 
 br_s2  = seq(Landmark, 14,  length=99)
 sfalb2<- make_sf(    (br_s2[2]-br_s2[1])/4 , arg1)
 tHor <- 1.5
 auc.hq.use<-auc.hqm(pbcT1, sfalb2, Landmark,tHor,  
              event_time_name = 'years', status_name = 'status2')
 auc.hq.use
}
