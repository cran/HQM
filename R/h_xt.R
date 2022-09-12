
h_xt <- function(br_X, br_s, int_X, size_s_grid, alpha, x,t, b, Yi,n){


  h1 = matrix(int_X, size_s_grid, n)

  t_index = findInterval(t, br_s)

  int_X_s = as.vector(h1[1:(size_s_grid-t_index),])
  int_X_s_t = as.vector(h1[(t_index+1):size_s_grid,])


  Yi_cut <-Yi[(t_index+1):size_s_grid,]

  Y_s_t = as.vector(Yi_cut )


  K_s_x = K_b(b, x, br_X, Epan)[int_X_s]

  a_s_t = alpha[int_X_s_t]


  h_nom = sum(Y_s_t*a_s_t*K_s_x)

  h_denom = sum(Y_s_t*K_s_x)

  h_tx = h_nom/h_denom
  return(h_tx)

}
