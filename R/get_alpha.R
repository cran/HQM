get_alpha <- function(N, Y, b, br_X, K=Epan ){

  a_nom = K_b_mat(b,br_X, br_X, K)   %*% colSums(N)

  a_denom =   K_b_mat(b,br_X, br_X, K) %*% colSums(Y)

  al = a_nom/a_denom

  return(al)

}
