index_optim <- function(in.par, data, data.id, br_s, X1, XX1,  
                        event_time_name = 'years', time_name = 'year', event_name = 'status2',
                        b, t, true.haz) {
  n = length(data.id$id)
  #size_s_grid <- size_X_grid <- 100
  length.s<- length(br_s) 
  X  <- Reduce(`+`, Map(`*`, in.par, X1)) # in.par[1]*X1+ in.par[2]*X2
  XX =  Reduce(`+`, Map(`*`, in.par, XX1)) # in.par[1]*XX1+ in.par[2]* XX2
  
  #X = in.par[1]*X1+ in.par[2]*X2
  #XX = in.par[1]*XX1+ in.par[2]* XX2

  s = data[, time_name]
  ss <- data.id[, event_time_name]

  delta <- data.id[, event_name]
  br_X = seq(min(X), max(X), (max(X)-min(X))/( length.s-1))
  #br_s = seq(0, max(s), max(s)/( length.s-1))
  X_lin = lin_interpolate(br_s, data.id$id, data$id, X, s)

  int_X <- findInterval(X_lin, br_X)
  int_s = rep(1:length(br_s), n)
  Y <- make_Y(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,
              length.s,length.s, int_s,int_X, event_time = event_time_name, n)
  N  <- make_N(data, data.id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
  Yi <- make_Yi(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,
                length.s,length.s, int_s,int_X, event_time = event_time_name, n)
  Ni = make_Ni(breaks_s=br_s, length.s, ss, delta, n)

  alpha <- get_alpha(N, Y, b, br_X, K=Epan )
  hqm.est <- h_xt_vec(br_X,br_s, length.s, alpha, t, b, Yi, int_X, n)
  hqm.est[is.na(hqm.est)] <- 0
  cv.score <- sum((hqm.est - true.haz)^2)
  return(cv.score)
}
