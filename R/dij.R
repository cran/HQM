dij  <- function(b,x,y, K){
  K_h = (1/b)*K((x-y)/b)*  (x-y)^2
  return(K_h)
}