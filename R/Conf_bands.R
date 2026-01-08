Conf_bands <- function(data,   marker_name, event_time_name = 'years', time_name = 'year', event_name = 'status2', x, b){

  ##############################################################################
  data.id = to_id(data)
  n = length(data.id$id)
  size_s_grid <- size_X_grid <- 100

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
  ##############################################################################
  alpha<-get_alpha(N, Y, b, br_X, K=Epan )
  l_t = (length(br_s)-1)
  Boot = matrix(1, n, l_t)
  sBoot = matrix(1, n, l_t)
  sigma = 1:l_t
  sigma_2 = 1:l_t
  h = 1:l_t

  for(i in 1:l_t)
  {
    Booti = 1:n
    g = g_xt( br_X, br_s, size_s_grid, int_X, x, br_s[i], b, Yi, Y, n)
    Boot_all = prep_boot(g, alpha, Ni, Yi, size_s_grid, br_X, br_s, br_s[i], b, int_X, x, n)

    A_1 = Boot_all[[1]]
    A_2 = Boot_all[[2]]
    B = Boot_all[[3]]
    h[i] = Boot_all[[4]]
    Gamma = Boot_all[[5]]
    A_B = (A_1 - A_2)/Gamma
    for(j in 1:n)
    {
      Vi = rnorm(n)
      Booti[j] = sum(Vi*(A_B + B))/sqrt(sum((A_B + B)^2))
    }
    if(is.nan(sum(Booti)) == TRUE ){ Booti = replicate(n,0)}
    sBoot[,i] = sort(Booti)
    Boot[,i] = Booti
    sigma[i] = sum((A_B + B)^2)
    sigma_2[i] = var(Boot[,i])
  }
  mBoot = 1:l_t
  for(i in 1:l_t){  mBoot[i] = sort(abs(Boot[,i]))[(n*0.95)]  }
  W_N = max(mBoot)

  nBoot = 1:n
  for(i in 1:n){  nBoot[i] = max(abs(Boot[i,])) }
  W_Nn = sort(nBoot)[(n*0.95)]
  I_p_up = h - sqrt(sigma)*sBoot[(n*0.025),]/sqrt(n)
  I_p_do = h - sqrt(sigma)*sBoot[(n*0.975),]/sqrt(n)

  I_u_up = h + sqrt(sigma)*W_N/sqrt(n)
  I_u_do = h - sqrt(sigma)*W_N/sqrt(n)

  I_nu = h + sqrt(sigma)*W_Nn/sqrt(n)
  I_nd = h - sqrt(sigma)*W_Nn/sqrt(n)

  return(list(Boot = Boot, lsBoot = sBoot[(n*0.975),], usBoot = sBoot[(n*0.025),], mBoot = W_N, sigma_2 = sigma_2, sigma = sigma, h_hat = h, I_p_up = I_p_up, I_p_do = I_p_do, I_u_up = I_u_up, I_u_do = I_u_do, I_nd = I_nd,I_nu = I_nu ))
}
