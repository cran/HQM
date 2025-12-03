#' Boot.hqm
#'
#' Compute bootstrap hazard estimate for a single bootstrap sample.
#'
#' @param in.par numeric indexing parameters length 2
#' @param data data frame (bootstrap sample)
#' @param data.id id-level data frame (result of to_id)
#' @param X1,XX1,X2,XX2 numeric vectors
#' @param event_time_name time-to-event column name
#' @param time_name observation time column name
#' @param event_name event indicator name
#' @param b bandwidth
#' @param t evaluation point (scalar or vector)
#' @return numeric vector with estimated hazard on the grid
#' @export
Boot.hqm <- function(in.par, data, data.id, X1, XX1, X2, XX2,
                     event_time_name = 'years', time_name = 'year', event_name = 'status2',
                     b, t) {
  n = length(data.id$id)
  size_s_grid <- size_X_grid <- 100

  X = in.par[1]*X1+ in.par[2]*X2
  XX = in.par[1]*XX1+ in.par[2]* XX2

  s = data[, time_name]
  ss <- data.id[, event_time_name]

  delta <- data.id[, event_name]
  br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
  br_s = seq(0, max(s), max(s)/( size_s_grid-1))
  X_lin = lin_interpolate_plus(br_s, data.id$id, data$id, X, s)

  int_X <- findInterval(X_lin, br_X)
  int_s = rep(1:length(br_s), n)
  Y <- make_Y(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,
              size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  N  <- make_N(data, data.id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
  Yi <- make_Yi(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,
                size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
  Ni = make_Ni(breaks_s=br_s, size_s_grid, ss, delta, n)

  alpha <- get_alpha(N, Y, b, br_X, K=Epan )
  hqm.est <- h_xt_vec(br_X,br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
  hqm.est[is.na(hqm.est)] <- 0
  hqm.est
}
