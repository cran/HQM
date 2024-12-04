bs.hqm<-function(xin, est, landm, th, event_time_name = 'years', status_name = 'status2')
  #xin: data frame, e.g. pbc2
  #est - estimator, the output of 'get_h_x'
  #landm: landmark time, user defined
  #th: 1.5
  # event_time_name - name of event times in xin data frame
  # status_name - name of status variable in xin data frame
{
  xin.use<- xin[,event_time_name]-landm
  delta.use<-xin[,status_name]
  surv.hqm<-approx(est, xout=sort(xin.use, decreasing = TRUE))$y
  surv.hqm2<- surv.hqm[rank(xin.use)]
  surv.hqm2[is.na(surv.hqm2) ]<-0.1
  event.times<-xin[,event_time_name]
  status.var<-xin[,status_name]
  BS.hqm <-pec(object = cbind(1, surv.hqm2), formula = Surv(event.times, status.var) ~ 1,
               data =  xin, cens.model = "marginal",cause=1,
               exact = FALSE, times =c(0,th), ptime = 0)$AppErr$matrix[2]
  BS.hqm
}