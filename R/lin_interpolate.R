

lin_interpolate <- function(t, i, data_id, data_marker, data_time){
  X = sapply(i, function(id){
    t.points = data_time[data_id == id]
    M.points = data_marker[data_id == id]
    if(length(M.points) > 1) {ifelse( t > max(t.points),tail(M.points,n=1) , approxfun(t.points, M.points)(t) )}
    else{matrix(M.points,length(t),1)}

  })
  return(X)
}
