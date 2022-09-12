h_xt_vec <- function(br_X, br_s, size_s_grid, alpha, t, b, Yi, int_X, n){

  t_index = findInterval(t, br_s)

  h1 = matrix(int_X, size_s_grid, n)
  int_X_s = as.vector(h1[1:(size_s_grid-t_index),])
  int_X_s_t = as.vector(h1[(t_index+1):size_s_grid,])

  Yi_cut <-Yi[(t_index+1):size_s_grid,]

  Y_s_t = as.vector(Yi_cut )
  a_s_t = alpha[int_X_s_t]

  stable_part = Y_s_t*a_s_t

  K_s_t_mat = K_b_mat(b,br_X, br_X, Epan)[,int_X_s]

  h_vec_nom = K_s_t_mat %*% stable_part
  h_vec_denom = K_s_t_mat %*% Y_s_t

  h_vec = h_vec_nom/h_vec_denom

  return(h_vec)

}
