Boot.hrandindex.param<-function(B, Boot.samples, marker_name1, marker_name2, event_time_name, time_name, 
                                event_name, b, t, true.haz, v.param, n.est.points)
{
  Mat.boot.haz.rate<-matrix(nrow=n.est.points, ncol=B) 
  for(k in 1:B)
  {
    data.use<-Boot.samples[[k]]
    data.use.id<-to_id(data.use)
    data.use.id<-data.use.id[complete.cases(data.use.id), ]
    X1t=data.use[,marker_name1] -mean(data.use[, marker_name1])
    XX1t=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
    X2t=data.use[,marker_name2]  -mean(data.use[, marker_name2])
    XX2t=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])
    s = data.use[,time_name]
    br_s = seq(0, max(s), max(s)/( n.est.points-1))
    #cat("br_s =",  br_s, "\n")
    X1=list(X1t, X2t)
    XX1=list(XX1t, XX2t)
    
    res<- optim(par=v.param, fn=index_optim,  data=data.use, data.id=data.use.id, br_s = br_s, X1=X1, XX1=XX1,
                event_time_name = event_time_name, time_name = time_name,
                event_name = event_name,   b=b, t=t, true.haz=true.haz, 
                method="Nelder-Mead") 
    boot.haz<-Boot.hqm (c(res$par[1],res$par[2]), data.use, data.use.id, n.est.points, X1, XX1, event_time_name = 'years', time_name = 'year', event_name = 'status2',  b, t)
    Mat.boot.haz.rate[,k]<- boot.haz  
  }
   Mat.boot.haz.rate 
}