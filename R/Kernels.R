
Epan<-function(x) { return(((3/4)*(1-(x)^2))*(abs(x)<1))}

K_b <- function(b,x,y, K){
  K_h = (1/b)*K((x-y)/b)
  return(K_h)
}

K_b_mat <- function(b,x,y, K){
  x_mat = replicate(length(x), x)
  y_mat = t(replicate(length(y), y))
  K_h = (1/b)*K((x_mat-y_mat)/b)
  return(K_h)
}

