
g_xt <- function(br_X, br_s, size_s_grid, int_X, x, t, b, Yi, Y, n){

  E_inv = 1/(K_b_mat(b,br_X, br_X, Epan) %*% colSums(Y))

  t_index = findInterval(t, br_s)

  h1 = matrix(int_X, size_s_grid, n)
  int_X_s = as.vector(h1[1:(size_s_grid-t_index),])
  int_X_s_t = as.vector(h1[(t_index+1):size_s_grid,])

  Yi_cut <-Yi[(t_index+1):size_s_grid,]

  Y_s_t = as.vector(Yi_cut )
  K_s_x = K_b(b, x, br_X, Epan)[int_X_s]
  E_s_t = E_inv[int_X_s_t]

  stable_part = Y_s_t*E_s_t*K_s_x

  K_s_t_mat = K_b_mat(b,br_X, br_X, Epan)[,int_X_s_t]

  g = K_s_t_mat %*% stable_part

  return(g)

}
