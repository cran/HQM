
make_N <- function(data, data.id, breaks_X, breaks_s, ss, XX, delta){

  N_mat = matrix(0,length(breaks_s),length(breaks_X))

  N_pos_s = findInterval(ss, breaks_s)
  N_pos_X = findInterval(XX, breaks_X)

  N_h = cbind(N_pos_s, N_pos_X, delta)

  for(i in 1:nrow(N_h)){
    N_mat[N_h[i,1],N_h[i,2]] = N_mat[N_h[i,1],N_h[i,2]]+N_h[i,3]
  }

  return(N_mat)
}

make_Ni <- function(breaks_s, size_s_grid, ss, delta, n){
  N = matrix(0,size_s_grid , n)

  N_pos_s = findInterval(ss, breaks_s)

  for(j in 1:n){
    N[N_pos_s[j],j] = N[N_pos_s[j],j] + delta[j]
  }

  return(N)
}

make_Y <- function(data, data.id, X_lin, breaks_X, breaks_s, size_s_grid, size_X_grid, int_s, int_X, event_time = 'years', n){

  Y = matrix(0,length(breaks_s), n)

  for(i in 1:n){
    Y_i = rep(data.id[i,event_time], size_s_grid)- breaks_s
    Y_i[Y_i > 0 &  Y_i > max(breaks_s)/(size_s_grid-1)] = max(breaks_s)/(size_s_grid-1)
    Y_i[Y_i <0] = 0
    Y[,i] = Y_i
  }

  Y_help = as.vector(Y)
  a = cbind(int_s, int_X, Y_help)


  Y_mat = matrix(0,size_s_grid,size_X_grid )

  for(i in 1:length(a[,1])){
    Y_mat[a[i,1],a[i,2]] = Y_mat[a[i,1],a[i,2]]+a[i,3]
  }

  return(Y_mat)
}


make_Yi <- function(data, data.id, X_lin, breaks_X, breaks_s,size_s_grid,size_X_grid, int_s,int_X, event_time = 'years', n){
  Y = matrix(0,size_s_grid , n)

  for(j in 1:n){
    Y_i = rep(data.id[j,event_time], size_s_grid )- breaks_s[1:size_s_grid ]
    Y_i[Y_i > 0 &  Y_i > max(breaks_s)/(size_s_grid - 1)] = max(breaks_s)/(size_s_grid - 1)
    Y_i[Y_i <0] = 0
    Y[,j] = Y_i
  }

  return(Y)
}
