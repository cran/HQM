StudentizedBwB.Index.CIs<-function(n.est.points, all.mat, time.grid, hqm.est, a.sig)
{
  Mat.boot.haz.rate <- all.mat$Mat
  Mat.boot.haz.rate.sd <- all.mat$Mat.sd
  Log.Mat.boot.haz.rate <-  all.mat$Log.Mat 
  Log.Mat.boot.haz.rate.sd <-  all.mat$Log.Mat.sd
    
  t.stat025<-matrix(nrow=n.est.points, ncol=1)  
  t.stat975<-matrix(nrow=n.est.points, ncol=1) 
  t.stat95<-matrix(nrow=n.est.points, ncol=1) 
  
  
  LogSDMat.i<-vector(length=n.est.points, mode="numeric")
  SDMat<-matrix(nrow=n.est.points, ncol=1)
  LogSDMat<-vector(length=n.est.points, mode="numeric")
  
  # Plain HR CIs and symmetric CI version
  t.stat.tmp <- Mat.boot.haz.rate 
  for(s1 in 1:n.est.points)
  {
    t.stat025[s1, 1]<- quantile(t.stat.tmp[s1,], a.sig/2, na.rm=TRUE)
    t.stat975[s1, 1]<- quantile(t.stat.tmp[s1,], 1-a.sig/2, na.rm=TRUE)
    t.stat95[s1, 1]<- quantile(abs(t.stat.tmp[s1,]), a.sig, na.rm=TRUE)
    SDMat[s1,]<-sd(Mat.boot.haz.rate.sd[s1,], na.rm=TRUE)
  } 
  
  DoCI<- hqm.est -  SDMat*t.stat025
  UpCI<- hqm.est -  SDMat* t.stat975 
  SymDoCI<- hqm.est -  SDMat*t.stat95
  SymUpCI<- hqm.est +  SDMat* t.stat95
   
  #Log CI
  log.t.stat025<-matrix(nrow=n.est.points, ncol=1) 
  log.t.stat975<-matrix(nrow=n.est.points, ncol=1)   
  log.t.stat95<-matrix(nrow=n.est.points, ncol=1)  
  
  log.t.stat.tmp <- Log.Mat.boot.haz.rate 
  for(s1 in 1:n.est.points)
  {
    log.t.stat025[s1, 1]<- quantile(log.t.stat.tmp[s1,], probs=c(a.sig/2), na.rm=TRUE)
    log.t.stat975[s1, 1]<- quantile(log.t.stat.tmp[s1,], 1-a.sig/2, na.rm=TRUE)
    log.t.stat95[s1, 1]<- quantile(abs(log.t.stat.tmp[s1,]), a.sig, na.rm=TRUE)
    LogSDMat[s1]<-sd(Log.Mat.boot.haz.rate.sd[s1,], na.rm=TRUE)
  } 
  
  LogDoCI<-     log(hqm.est) -  LogSDMat*log.t.stat025
  LogUpCI<-     log( hqm.est) -  LogSDMat* log.t.stat975 
  LogSymDoCI<-    log( hqm.est)-  LogSDMat*log.t.stat95
  LogSymUpCI<-      log( hqm.est) + LogSDMat* log.t.stat95
  
  ### Log interval
  LogTDoCI<-      hqm.est  * exp( -LogSDMat*log.t.stat025 )
  LogtUpCI<-      hqm.est  * exp( -LogSDMat*log.t.stat975 )
  SymLogTDoCI<-   hqm.est  * exp( -LogSDMat*log.t.stat95  )    
  SymLogtUpCI<-   hqm.est  * exp( LogSDMat*log.t.stat95  )   
  
  data.out<-data.frame(time=time.grid, est=hqm.est, DoCI=DoCI, UpCI=UpCI, SymDoCI=SymDoCI, SymUpCI=SymUpCI, 
                       LogDoCI = LogDoCI, LogUpCI = LogUpCI, LogSymDoCI = LogSymDoCI, LogSymUpCI = LogSymUpCI, 
                       log.est = log(hqm.est), LogTDoCI = LogTDoCI, LogtUpCI= LogtUpCI, 
                       SymLogTDoCI = SymLogTDoCI, SymLogtUpCI = SymLogtUpCI)
  
}