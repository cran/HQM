get_h_x <- function(data, marker_name, event_time_name = 'years', time_name = 'year', event_name = 'status2', x, b){

  size_s_grid <- size_X_grid <- 100
  data.id = to_id(data)
  n = length(data.id$id)

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

  alpha<-get_alpha(N, Y, b, br_X, K=Epan )

  h_c<-sapply(br_s[1:99], function(t){
    h_xt(br_X, br_s, int_X, size_s_grid, alpha, x,t, b, Yi, n)
  })
  return(h_c)
}
