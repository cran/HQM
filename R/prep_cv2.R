prep_cv2 <- function(in.par, data, data.id, marker_name1, marker_name2,  event_time_name = 'years',  time_name = 'year',
                     event_name = 'status2', n, I, b)
{
  
  data_split_list = dataset_split(I, data)
  data_split_list_id = dataset_split(I, data.id)
  
  K = length(data_split_list)
  
  h_xt_mat_list = list()
  
  size_s_grid <- size_X_grid <- 100
  
  for(j in 1:K){
    
    X1 = data_split_list[[j]][,marker_name1]
    XX1 = data_split_list_id[[j]][,marker_name1]
    
    X2 = data_split_list[[j]][,marker_name2]
    XX2 = data_split_list_id[[j]][,marker_name2]
    
    X = in.par[1]*X1+ in.par[2]*X2
    XX= in.par[1]*XX1+ in.par[2]* XX2
    #cat("sum = ", sum(X), "\n")
    s = data_split_list[[j]][, time_name]
    ss <- data_split_list_id[[j]][, event_time_name]
    
    delta <- data_split_list_id[[j]][, event_name]
    
    br_X = seq(min(X), max(X), (max(X)-min(X))/( size_X_grid-1))
    br_s = seq(0, max(s), max(s)/( size_s_grid-1))
    
    X_lin = lin_interpolate(br_s, data_split_list_id[[j]]$id, data_split_list[[j]]$id, X, s)
    
    nj = length(data_split_list_id[[j]]$id)
    
    int_X <- findInterval(X_lin, br_X)
    
    int_s = rep(1:length(br_s), nj)
    
    Y <- make_Y(data_split_list[[j]], data_split_list_id[[j]], X_lin, breaks_X=br_X, breaks_s=br_s,size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, nj)
    N  <- make_N(data_split_list[[j]], data_split_list_id[[j]], breaks_X=br_X, breaks_s=br_s, ss, XX, delta)
    
    alpha<-get_alpha(N, Y, b, br_X, K=Epan )
    
    Yi <- make_Yi(data_split_list[[j]], data_split_list_id[[j]], X_lin, breaks_X=br_X, breaks_s=br_s,size_s_grid,size_X_grid, int_s,int_X, event_time = event_time_name, nj)
    Ni = make_Ni(breaks_s=br_s, size_s_grid, ss, delta, n)
    
    h_xt_mat = sapply(br_s[1:99], function(si){h_xt_vec( br_X, br_s, size_s_grid, alpha, si, b, Yi, int_X, nj)})
    
    
    h_xt_mat_list[[j]] = t(h_xt_mat)
  }
  
  return(h_xt_mat_list)
  
}
