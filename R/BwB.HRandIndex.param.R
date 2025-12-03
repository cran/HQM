BwB.HRandIndex.param<-function(B, B1, Boot.samples, marker_name1, marker_name2, event_time_name, time_name, 
                                event_name, b, t, true.haz, v.param, hqm.est, id, xin)
{
  n.est.points <- length(hqm.est)
  Mat.boot.haz.rate<-matrix(nrow=n.est.points, ncol=B)
  Log.Mat.boot.haz.rate<-matrix(nrow=n.est.points, ncol=B) 
  Mat.boot.haz.rate.sd<-matrix(nrow=n.est.points, ncol=B) 
  Log.Mat.boot.haz.rate.sd<-matrix(nrow=n.est.points, ncol=B) 
  nn<-max(  as.double(xin[, id]) )
  
  for(k in 1:B)
  {
    data.use<-Boot.samples[[k]]
    data.use.id<-to_id(data.use)
    data.use.id<-data.use.id[complete.cases(data.use.id), ]
    X1=data.use[,marker_name1] -mean(data.use[, marker_name1])
    XX1=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
    X2=data.use[,marker_name2]  -mean(data.use[, marker_name2])
    XX2=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])
    
    res <- optim(par = v.param, fn = index_optim, data = data.use, data.id = data.use.id, X1 = X1, 
                 XX1 = XX1, X2 = X2,  XX2 = XX2, event_time_name = event_time_name, time_name = time_name,
                 event_name = event_name, b = b, t = t, true.haz = true.haz,  method = "Nelder-Mead" )
    
    boot.haz<-Boot.hqm(c(res$par[1],res$par[2]), data.use, data.use.id, X1, XX1, X2, XX2, event_time_name = 'years', 
                                                    time_name = 'year', event_name = 'status2', b, t)
    Mat.boot.haz.rate.i<-matrix(nrow=n.est.points, ncol=B1) 
    
    Boot.samples.i<-list()
    for(jj in 1:B1)
    {
      ii.use<-c()
      iid.use<-c()
      index.nni <- sample (nn, replace = TRUE)  
      for(ll in 1:nn)
      {
        ii.use2<-which(xin[,id]==index.nni[ll])
        ii.use<-c(ii.use, ii.use2)
        iid.use2<-rep(index.nni[ll], times=length(ii.use2))
        iid.use<-c(iid.use, iid.use2)
      }
      xin.ii<-xin[ii.use,]
      Boot.samples.i[[jj]]<- xin.ii[order(xin.ii$id),]  
    }
    
    for(j1 in 1:B1)
    {
      data.use.j1<-Boot.samples.i[[j1]]
      data.use.id.j1<-to_id(data.use.j1)
      data.use.id.j1<-data.use.id.j1[complete.cases(data.use.id.j1), ]
      X1.j1=data.use.j1[,marker_name1] -mean(data.use.j1[, marker_name1])
      XX1.j1=data.use.id.j1[,marker_name1] -mean(data.use.id.j1[, marker_name1])
      X2.j1=data.use.j1[,marker_name2]  -mean(data.use.j1[, marker_name2])
      XX2.j1=data.use.id.j1[,marker_name2] -mean(data.use.id.j1[, marker_name2])
      
      res <- optim(par = v.param, fn = index_optim, data = data.use.j1,  data.id = data.use.id.j1, X1 = X1.j1,
                    XX1 = XX1.j1, X2 = X2.j1, XX2 = XX2.j1,  event_time_name = event_time_name,  
                    time_name = time_name, event_name = event_name,  b = b, t = t,  true.haz = true.haz, 
                    method = "Nelder-Mead")
      
      boot.haz.i<-Boot.hqm (c(res$par[1],res$par[2]), data.use.j1, data.use.id.j1, X1.j1, XX1.j1, X2.j1, XX2.j1,  
                              event_time_name = 'years', time_name = 'year', event_name = 'status2',   b, t)
      Mat.boot.haz.rate.i[,j1]<-boot.haz.i
    }
    SDMat.i<-vector(length=n.est.points, mode="numeric")
    LogSDMat.i<-vector(length=n.est.points, mode="numeric")
    for(m1 in 1: n.est.points)
    {
      SDMat.i[m1]<-sd(Mat.boot.haz.rate.i[m1,], na.rm=TRUE)
    }
    Mat.boot.haz.rate[,k]<- ( boot.haz  - hqm.est )/SDMat.i
    Mat.boot.haz.rate.sd[,k]<- boot.haz
    
    for(m1 in 1:n.est.points)
    {
      LogSDMat.i[m1]<-sd(log(Mat.boot.haz.rate.i[m1,]), na.rm=TRUE)
    } 
    
    Log.Mat.boot.haz.rate[,k] <- (log(boot.haz) - log(hqm.est))/LogSDMat.i
    Log.Mat.boot.haz.rate.sd[,k] <- log(boot.haz) 
  }
  
   list(Mat = Mat.boot.haz.rate,  Mat.sd=Mat.boot.haz.rate.sd, 
        Log.Mat= Log.Mat.boot.haz.rate, Log.Mat.sd=Log.Mat.boot.haz.rate.sd)
}