#' SingleIndCondFutHaz
#'
#' Compute the indexed local linear future conditional hazard rate estimator on a grid.
#'
#' @param datain data frame with longitudinal observations (wide/tall format used by the script)
#' @param id column name (string) of the id variable in datain
#' @param marker_name1 first marker name (string)
#' @param marker_name2 second marker name (string)
#' @param event_time_name name of event time column (default 'years')
#' @param time_name name of observation time column (default 'year')
#' @param event_name name of event indicator column (default 'status2')
#' @param in.par numeric vector of length 2 with indexing parameters
#' @param b bandwidth
#' @return data.frame with columns time and est (estimated hazard) on a grid
#' @export
SingleIndCondFutHaz <- function(datain, id, marker_name1, marker_name2,
                                event_time_name = 'years', time_name = 'year',
                                event_name = 'status2', in.par, b, t) {
  datain_id = to_id(datain)
  size_s_grid <- size_X_grid <- 100
  n = max(as.numeric(datain[, id]))
  s = datain[, time_name]
  ss <- datain_id[, event_name]
  br_s = seq(0, max(s), max(s)/( size_s_grid-1))
  int_s = rep(1:length(br_s), n)
  delta <- datain_id[, event_name]

  X1 = datain[, marker_name1] - mean(datain[, marker_name1])
  XX1 = datain_id[, marker_name1] - mean(datain_id[, marker_name1])
  X2 = datain[, marker_name2] - mean(datain[, marker_name2])
  XX2 = datain_id[, marker_name2] - mean(datain_id[, marker_name2])

  par.x1 <- in.par[1]
  par.x2 <- in.par[2]

  X = par.x1 * X1 + par.x2 * X2
  XX = par.x1 * XX1 + par.x2 * XX2

  br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))

  X_lin = lin_interpolate_plus(br_s, datain_id[, id], datain[, id], X, s)
  int_X <- findInterval(X_lin, br_X)
  N <- make_N(datain, datain_id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
  Y <- make_Y(datain, datain_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X, event_time_name, n)
  alpha <- get_alpha(N, Y, b, br_X, K=Epan )
  Yi <- make_Yi(datain, datain_id, X_lin, br_X, br_s, size_s_grid, size_X_grid, int_s, int_X, event_time_name, n)
  est <- h_xt_vec(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n)
  data.frame(time=br_s, est=est)
}
