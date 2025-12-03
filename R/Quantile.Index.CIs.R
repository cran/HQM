Quantile.Index.CIs<-function(B, n.est.points, Mat.boot.haz.rate, time.grid, hqm.est, a.sig)
{
  # Plain Log + symmetric CIs
  QuantMat<-matrix(nrow=n.est.points, ncol=2)
  SymQuantMat<-matrix(nrow=n.est.points, ncol=1)
  for(m in 1:n.est.points)
  {
    QuantMat[m,]<-quantile(Mat.boot.haz.rate[m,], type=7, probs = c(a.sig/2, 1-a.sig/2)) #- boot.haz
    SymQuantMat[m,]<-quantile(Mat.boot.haz.rate[m,], type=7, probs = c(1-a.sig)) #- boot.haz
  }
  QuantMat<- QuantMat-matrix(hqm.est, nrow=n.est.points, ncol=ncol(QuantMat))
  SymQuantMat <- abs(SymQuantMat-matrix(hqm.est, nrow=n.est.points, ncol=ncol(SymQuantMat)))
  
  DoCI<-   hqm.est -  QuantMat[,2]  
  UpCI<-    hqm.est -    QuantMat[,1]  
  SymDoCI<-  hqm.est -  SymQuantMat 
  SymUpCI<-  hqm.est +    SymQuantMat
  #Log Version + symmetric CIs
  
  LogQuantMat<-matrix(nrow=n.est.points, ncol=2)
  LogQuantMat.tmp <- log( Mat.boot.haz.rate ) - log(matrix(hqm.est, nrow=n.est.points, ncol=B))
  SymLogQuantMat<-matrix(nrow=n.est.points, ncol=1)
  
  LogQuantMat.sort<-matrix(nrow=n.est.points, ncol=2)
  SymLogQuantMat.sort<-matrix(nrow=n.est.points, ncol=1)
  
  for(m in 1:n.est.points)
  {
    LogQuantMat[m,]<-quantile(LogQuantMat.tmp[m,], type=7, probs = c(a.sig/2, 1-a.sig/2))  
    SymLogQuantMat[m,]<-quantile(abs(LogQuantMat.tmp[m,]), type=7, probs = c(1-a.sig))   
  }
  
  LogDoCI<-      hqm.est *exp( - LogQuantMat[,2] ) 
  LogUpCI<-     hqm.est *exp( - LogQuantMat[,1] )
  SymLogDoCI<-   hqm.est *exp( - SymLogQuantMat ) 
  SymLogUpCI<-   hqm.est *exp(  SymLogQuantMat  ) 
  
  LogQuantMat<-matrix(nrow=n.est.points, ncol=2)
  LogQuantMat.tmp <- log( Mat.boot.haz.rate )  
  SymLogQuantMat<-matrix(nrow=n.est.points, ncol=1)
  SymLogQuantMat.tmp <- abs(log( Mat.boot.haz.rate )  - log(matrix(hqm.est, nrow=n.est.points, ncol=B)))
  
  LogQuantMat.sort<-matrix(nrow=n.est.points, ncol=2)
  SymLogQuantMat.sort<-matrix(nrow=n.est.points, ncol=1)
  
  for(m in 1:n.est.points)
  {
    LogQuantMat[m,]<-quantile(LogQuantMat.tmp[m,], type=7, probs = c(a.sig/2, 1-a.sig/2))  
    SymLogQuantMat[m,]<-quantile(abs(SymLogQuantMat.tmp[m,]), type=7, probs = c(1-a.sig))   
  }
  tLogDoCI<-     2*log(hqm.est)  - LogQuantMat[,2]   
  tLogUpCI<-     2*log(hqm.est)  - LogQuantMat[,1] 
  tSymLogDoCI<-  log(hqm.est)  - SymLogQuantMat    
  tSymLogUpCI<-  log(hqm.est) +  SymLogQuantMat   
  
  data.out<-data.frame(time=time.grid, est=hqm.est, downci=DoCI, upci=UpCI, docisym=SymDoCI, upcisym=SymUpCI, 
                       logdoci = LogDoCI, logupci = LogUpCI, logdocisym = SymLogDoCI, logupcisym = SymLogUpCI, 
                       log.est = log(hqm.est), tLogDoCI = tLogDoCI, tLogUpCI= tLogUpCI, tSymLogDoCI = tSymLogDoCI, tSymLogUpCI = tSymLogUpCI)
}