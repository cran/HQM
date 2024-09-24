llK_b<-function(b,x,y, K)
{ 
  sn0 <- sn.0(y,x, b, K)
  sn1 <- sn.1(y,x, b, K)
  sn2 <- sn.2(y,x, b, K)
  
  kuse<-K_b(b,x,y, K)
  
  numer<- kuse *sn2 - sn1 * kuse * ((x-y)/b)
  
  denom<- sn0*sn2 - sn1^2
  numer/denom
}