
K_b <- function(b,x,y, K){
  K_h = (1/b)*K((x-y)/b)
  return(K_h)
}


xK_b  <- function(b,x,y, K){
  K_h = (1/b)*K((x-y)/b)*  (x-y) 
  return(K_h)
}
 

K_b_mat <- function(b,x,y, K){
  x_mat = replicate(length(x), x)
  y_mat = t(replicate(length(y), y))
  K_h = (1/b)*K((x_mat-y_mat)/b)
  return(K_h)
}

 



