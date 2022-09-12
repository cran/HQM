
to_id = function(data_set){

  d_h = matrix(0,max(as.double((data_set$id))), length(data_set[1,]))
  for(i in 1:max(as.double(data_set$id))){
    h1 = data_set[data_set$id == i,]
    d_h[i,] = as.double(h1[length(h1[,1]),])
  }

  d_h = as.data.frame(d_h)
  colnames(d_h) <-  names(data_set)

  return(d_h)
}
