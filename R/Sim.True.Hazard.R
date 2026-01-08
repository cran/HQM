Sim.True.Hazard <- function(data.use, id, size_s_grid, marker_name1=marker_name1, marker_name2=marker_name2,
                            event_time_name=event_time_name, time_name=time_name, event_name=event_name,
                            in.par, b) {
  nn <- length(data.use)
   size_X_grid <- size_s_grid
  mat.alpha <- matrix(nrow=size_s_grid, ncol=nn)
  for(r in 1:nn) {
    data <- data.use[[r]]
    data.id = to_id(data)
    data.id <- data.id[complete.cases(data.id), ]
    n = length(data.id$id)
    X1 = data[, marker_name1] - mean(data[, marker_name1])
    XX1 = data.id[, marker_name1] - mean(data.id[, marker_name1])
    X2 = data[, marker_name2] - mean(data[, marker_name2])
    XX2 = data.id[, marker_name2] - mean(data.id[, marker_name2])

    X = in.par[1]*X1 + in.par[2]*X2
    XX = in.par[1]*XX1 + in.par[2]*XX2
    s = data[, time_name]
    ss <- data.id[, event_time_name]

    delta <- data.id[, event_name]
    br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
    br_s = seq(0, max(s), max(s)/( size_s_grid-1))
    X_lin = lin_interpolate(br_s, data.id$id, data$id, X, s)
    int_X <- findInterval(X_lin, br_X)
    int_s = rep(1:length(br_s), n)
    Y <- make_Y(data, data.id, X_lin, breaks_X=br_X, breaks_s=br_s,
                size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, n)
    N  <- make_N(data, data.id, breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
    alpha <- get_alpha(N, Y, b, br_X, K=Epan )
    mat.alpha[, r] <- alpha
  }
  tmp<- rowMeans(mat.alpha)
  tmp.mean <- mean(tmp[is.finite(tmp)])
   tmp[!is.finite(tmp)] <-tmp.mean
   tmp
}
