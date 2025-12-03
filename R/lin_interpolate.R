lin_interpolate <- function(t, i, data_id, data_marker, data_time)
{
  X = sapply(i, function(id){
    t.points = data_time[data_id == id]
    M.points = data_marker[data_id == id]
    
    ord <- order(t.points)
    t.points <- t.points[ord]
    M.points <- M.points[ord]
    # remove duplicates in t.points
    keep <- !duplicated(t.points)
    t.points <- t.points[keep]
    M.points <- M.points[keep]

    if(length(unique(t.points))==1) t.points[2]<-t.points[2]+0.01
    if(length(M.points) > 1) {ifelse( t > max(t.points),tail(M.points,n=1) , approxfun(t.points, M.points)(t) )}
    else{matrix(M.points,length(t),1)}

  })
  return(X)
}
