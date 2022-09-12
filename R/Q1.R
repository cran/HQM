Q1 <- function(h_xt_mat, int_X, size_X_grid, n, Yi){

  g_help = b_selection_prep_g(h_xt_mat, int_X, size_X_grid, n, Yi)

  return(sum(g_help* Yi, na.rm = TRUE))
}
