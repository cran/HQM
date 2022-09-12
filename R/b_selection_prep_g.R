b_selection_prep_g <- function(h_mat, int_X, size_X_grid, n, Yi){

  h = rbind(h_mat[, int_X], matrix(0,1,n*size_X_grid))^2
  v = (0:(n-1))*size_X_grid+1

  Y_h = do.call(rbind, replicate(size_X_grid, as.vector(Yi), simplify=FALSE))

  h = h*Y_h
  mat = h[,v]

  for(i in 2:size_X_grid){

    mat = mat + rbind(matrix(0,i-1,n),h[1:(size_X_grid-i+1),i+v-1])
  }

  return(mat)

}
