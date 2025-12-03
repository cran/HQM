Pivot.Index.CIs<-function(B, n.est.points, Mat.boot.haz.rate, time.grid, hqm.est, a.sig)
{
  
  PivotMat<-matrix(nrow=nrow(Mat.boot.haz.rate), ncol=2)
  PivotMat2<-matrix(nrow=nrow(Mat.boot.haz.rate), ncol=1)
  SDMat<-matrix(nrow=nrow(Mat.boot.haz.rate), ncol=1)
  SymPivotMat<-matrix(nrow=nrow(Mat.boot.haz.rate), ncol=1)
  
  for(m in 1: nrow(Mat.boot.haz.rate))
  {
    SDMat[m,]<-sd(Mat.boot.haz.rate[m,])
  }
  
  PivotMatUse <- Mat.boot.haz.rate - matrix(hqm.est, nrow=nrow(Mat.boot.haz.rate), ncol=ncol(Mat.boot.haz.rate))
  
  for(m in 1: nrow(Mat.boot.haz.rate))
  {
    PivotMat[m,]<-quantile(PivotMatUse[m,]/SDMat[m,], type=7, probs = c(0.025, 0.975)) 
    SymPivotMat[m,]<-quantile(Mat.boot.haz.rate[m,], type=7, probs = c(0.95)) 
  }
  
  PivotMat<- PivotMat-matrix(hqm.est, nrow=nrow(PivotMat), ncol=ncol(PivotMat))
  SymPivotMat <- abs(SymPivotMat-matrix(hqm.est, nrow=nrow(SymPivotMat), ncol=ncol(SymPivotMat)))
  
  DoCI<-     hqm.est -  PivotMat[,2] *SDMat 
  UpCI<-      hqm.est -    PivotMat[,1]  *SDMat   
  SymDoCI<-  hqm.est -  SymPivotMat 
  SymUpCI<-  hqm.est +    SymPivotMat
  
  # Log CIs
  LogPivotMat<-matrix(nrow=n.est.points, ncol=2)
  LogPivotMat.tmp <- log( Mat.boot.haz.rate ) - log(matrix(hqm.est, nrow=n.est.points, ncol=B))
  SymLogPivotMat<-matrix(nrow=n.est.points, ncol=1)
  
  LogPivotMat.sort<-matrix(nrow=n.est.points, ncol=2)
  SymLogPivotMat.sort<-matrix(nrow=n.est.points, ncol=1)
  
  for(m in 1:n.est.points)
  {
    LogPivotMat[m,]<-quantile(LogPivotMat.tmp[m,], type=7, probs = c(a.sig/2, 1-a.sig/2))  
    SymLogPivotMat[m,]<-quantile(abs(LogPivotMat.tmp[m,]), type=7, probs = c(1-a.sig))   
  }
  
  LogDoCI<-      hqm.est *exp( - LogPivotMat[,2] ) 
  LogUpCI<-     hqm.est *exp( - LogPivotMat[,1] )
  LogSymDoCI<-   hqm.est *exp( - SymLogPivotMat ) 
  LogSymUpCI<-   hqm.est *exp(  SymLogPivotMat  ) 
   
  LogPivotMat<-matrix(nrow=n.est.points, ncol=2)
  LogPivotMat.tmp <- log( Mat.boot.haz.rate )  
  SymLogPivotMat<-matrix(nrow=n.est.points, ncol=1)
  SymLogPivotMat.tmp <- abs(log( Mat.boot.haz.rate )  - log(matrix(hqm.est, nrow=n.est.points, ncol=B)))
  
  LogPivotMat.sort<-matrix(nrow=n.est.points, ncol=2)
  SymLogPivotMat.sort<-matrix(nrow=n.est.points, ncol=1)
  
  for(m in 1:n.est.points)
  {
    LogPivotMat[m,]<-quantile(LogPivotMat.tmp[m,], type=7, probs = c(a.sig/2, 1-a.sig/2))  
    SymLogPivotMat[m,]<-quantile(abs(SymLogPivotMat.tmp[m,]), type=7, probs = c(1-a.sig))   
  }
  
  LogTDoCI<-     2*log(hqm.est)  - LogPivotMat[,2]   
  LogtUpCI<-     2*log(hqm.est)  - LogPivotMat[,1] 
  SymLogTDoCI<-  log(hqm.est)  - SymLogPivotMat    
  SymLogtUpCI<-  log(hqm.est) +  SymLogPivotMat   
  
  
  data.out<-data.frame(time=time.grid, est=hqm.est, DoCI=DoCI, UpCI=UpCI, SymDoCI=SymDoCI, SymUpCI=SymUpCI, 
                       LogDoCI = LogDoCI, LogUpCI = LogUpCI, LogSymDoCI = LogSymDoCI, LogSymUpCI = LogSymUpCI, 
                       log.est = log(hqm.est), LogTDoCI = LogTDoCI, LogtUpCI= LogtUpCI, SymLogTDoCI = SymLogTDoCI, SymLogtUpCI = SymLogtUpCI)
  
}