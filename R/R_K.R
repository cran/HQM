R_K <- function(h_xt_mat_list, int_X, size_X_grid, Yi, Ni, n){

  K = length(h_xt_mat_list)
  Nu = n/K

  R = 0

  for(j in 1:K){
    a = b_selection_prep_g (h_xt_mat_list[[1]], int_X[(1:(Nu*size_X_grid))+(j-1)*Nu*size_X_grid], size_X_grid, Nu, Yi[,(1:Nu)+(j-1)*Nu])
    R = R + sum(a*Ni[,(1:Nu)+(j-1)*Nu], na.rm = T)
  }

  return(2*R)
}
