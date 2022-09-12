prep_boot <- function(g_xt, alpha, Ni, Yi, size_s_grid, br_X, br_s, t, b, int_X, x, n){

  g = g_xt[int_X]
  g = matrix(g, nrow = 100, ncol = n)

  a = alpha[int_X]
  a = matrix(a, nrow = 100, ncol = n)

  A_1 = colSums(g * Ni)/sqrt(n)
  A_2 = colSums(a*g*Yi)/sqrt(n)


  h1 = matrix(int_X, size_s_grid, n)
  t_index = findInterval(t, br_s)

  int_X_s = as.vector(h1[1:(size_s_grid-t_index),])
  int_X_s_t = as.vector(h1[(t_index+1):size_s_grid,])



  Yi_cut <-Yi[(t_index+1):size_s_grid,]
  Y_s_t = as.vector(Yi_cut )
  K_s_x = K_b(b, x, br_X, Epan)[int_X_s]
  a_s_t = alpha[int_X_s_t]


  W_B = colSums(matrix(Y_s_t*a_s_t*K_s_x, nrow = length(a_s_t)/n , ncol = n))
  Gamma = sum(Y_s_t*K_s_x)

  h_xt = sum(W_B)/Gamma


  B = (n*W_B/Gamma - h_xt)/sqrt(n)


  return(list(A_1, A_2, B, h_xt, Gamma))

}
