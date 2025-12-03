b_selection_index_optim <- function(in.par, data, marker_name1, marker_name2, event_time_name = 'years', time_name = 'year',
                                    event_name = 'status2', I, b)
{
  data.id = to_id(data)
  n = length(data.id$id)
  size_s_grid <- size_X_grid <- 100
  
  X1 = data[, marker_name1]
  XX1 = data.id[, marker_name1]
  X2 = data[, marker_name2]
  XX2 = data.id[, marker_name2]
  X = in.par[1]*X1+ in.par[2]*X2
  XX= in.par[1]*XX1+ in.par[2]* XX2
  
  s = data[, time_name]
  ss <- data.id[, event_time_name]
  
  delta <- data.id[, event_name]
  br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
  br_s = seq(0, max(s), max(s)/( size_s_grid-1))
  
  X_lin = lin_interpolate(br_s, data.id$id, data$id, X, s)
  
  int_X <- findInterval(X_lin, br_X)
  int_s = rep(1:length(br_s), n)
  
  Y <- make_Y(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  N  <- make_N(data, data.id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
  Yi <- make_Yi(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  Ni = make_Ni(breaks_s=br_s, size_s_grid, ss, delta, n)
  h_xt_mat_list = prep_cv2(in.par,  data, data.id, marker_name1, marker_name2, event_time_name = event_time_name,
                           time_name = time_name, event_name = event_name, n, I, b)
  alpha<-get_alpha(N, Y, b, br_X, K=Epan )
  h_xt_mat = t(sapply(br_s[1:99], function(si){h_xt_vec(br_X,br_s, size_s_grid, alpha, si, b, Yi, int_X, n)}))
  
  Q = Q1(h_xt_mat, int_X,  size_X_grid, n, Yi)
  R = R_K(h_xt_mat_list, int_X, size_X_grid, Yi, Ni, n)
  cv.score = Q-R
  return(cv.score)
}