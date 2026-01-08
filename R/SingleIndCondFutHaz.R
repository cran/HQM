SingleIndCondFutHaz <- function(datain, id, ls,  X1, XX1,
                                event_time_name = 'years', time_name = 'year',
                                event_name = 'status2', in.par, b, t) {
  datain_id = to_id(datain)
  size_s_grid <- size_X_grid <- ls
  n = max(as.numeric(datain[, id]))
  s = datain[, time_name]
  ss <- datain_id[, event_name]
  br_s = seq(0, max(s), max(s)/( size_s_grid-1))
  int_s = rep(1:length(br_s), n)
  delta <- datain_id[, event_name]
  
  X  <- Reduce(`+`, Map(`*`, in.par, X1)) # in.par[1]*X1+ in.par[2]*X2
  XX =  Reduce(`+`, Map(`*`, in.par, XX1)) # in.par[1]*XX1+ in.par[2]* XX2
  
  s = datain[, time_name]
  ss <- datain_id[, event_time_name]
  
  delta <- datain_id[, event_name]
  br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
  br_s = seq(0, max(s), max(s)/( size_s_grid-1))
  X_lin = lin_interpolate(br_s, datain_id$id, datain$id, X, s)
  
  int_X <- findInterval(X_lin, br_X)
  int_s = rep(1:length(br_s), n)
  Y <- make_Y(datain, datain_id, X_lin, breaks_X=br_X, breaks_s=br_s,
              size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  N  <- make_N(datain, datain_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
  Yi <- make_Yi(datain, datain_id, X_lin, breaks_X=br_X, breaks_s=br_s,
                size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  Ni = make_Ni(breaks_s=br_s, size_s_grid, ss, delta, n)
  
  alpha <- get_alpha(N, Y, b, br_X, K=Epan )
  hqm.est <- h_xt_vec(br_X,br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
  hqm.est[is.na(hqm.est)] <- 0
  data.frame(time=br_s, est=hqm.est)
}