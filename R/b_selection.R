b_selection <- function(data,   marker_name, event_time_name = 'years', time_name = 'year', event_name = 'status2', I, b_list){

  data.id = to_id(data)
  n = length(data.id$id)
  size_s_grid <- size_X_grid <-   100

  X = data[, marker_name]
  XX = data.id[, marker_name]

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

  b_results = 1:length(b_list)
  i = 1

  for(b in b_list){
    h_xt_mat_list = prep_cv(data, data.id, marker_name,event_time_name = event_time_name, time_name = time_name, event_name = event_name, n, I, b)

    alpha<-get_alpha(N, Y, b, br_X, K=Epan )
    h_xt_mat = t(sapply(br_s[1:(size_s_grid-1)], function(si){h_xt_vec(br_X,br_s, size_s_grid, alpha, si, b, Yi, int_X, n)}))


    Q = Q1(h_xt_mat, int_X,  size_X_grid, n, Yi)
    R = R_K(h_xt_mat_list, int_X, size_X_grid, Yi, Ni, n)

    b_results[i] = Q-R

    i = i+1

  }


  return(list(b_results, b_list))
}
