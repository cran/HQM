Boot.hrandindex.param<-function(B, Boot.samples, marker_name1, marker_name2, event_time_name, time_name, 
                                event_name, b, t, true.haz, v.param, n.est.points)
{
  Mat.boot.haz.rate<-matrix(nrow=n.est.points, ncol=B) 
  for(k in 1:B)
  {
    data.use<-Boot.samples[[k]]
    data.use.id<-to_id(data.use)
    data.use.id<-data.use.id[complete.cases(data.use.id), ]
    X1=data.use[,marker_name1] -mean(data.use[, marker_name1])
    XX1=data.use.id[,marker_name1] -mean(data.use.id[, marker_name1])
    X2=data.use[,marker_name2]  -mean(data.use[, marker_name2])
    XX2=data.use.id[,marker_name2] -mean(data.use.id[, marker_name2])
    
    res<- optim(par=v.param, fn=index_optim,  data=data.use, data.id=data.use.id, X1=X1, XX1=XX1,X2 = X2, XX2 =XX2, 
                event_time_name = event_time_name, time_name = time_name,
                event_name = event_name,   b=b, t=t, true.haz=true.haz, 
                method="Nelder-Mead") 
    boot.haz<-Boot.hqm (c(res$par[1],res$par[2]), data.use, data.use.id, X1, XX1, X2, XX2,  event_time_name = 'years', time_name = 'year', event_name = 'status2',   b, t)
    Mat.boot.haz.rate[,k]<- boot.haz  
  }
   Mat.boot.haz.rate 
}