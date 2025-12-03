#' lin_interpolate
#'
#' Interpolate marker values at grid times for each subject id.
#'
#' @param t numeric vector of target times (grid)
#' @param i vector of ids (repeated as in the original code)
#' @param data_id vector of ids aligned with data_marker and data_time
#' @param data_marker numeric vector of observed marker values aligned with data_id
#' @param data_time numeric vector of observation times aligned with data_marker
#' @return matrix of interpolated marker values (length(t) x number_of_ids)
#' @export
lin_interpolate_plus <- function(t, i, data_id, data_marker, data_time) {
  X = sapply(i, function(id){
    t.points = data_time[data_id == id]
    M.points = data_marker[data_id == id]
    if(length(unique(t.points))==1) t.points[2] <- t.points[2] + 0.01
    if(length(M.points) > 1) {
      ifelse(t > max(t.points), tail(M.points, n=1), approxfun(t.points, M.points)(t) )
    } else {
      matrix(M.points, length(t), 1)
    }
  })
  return(X)
}
