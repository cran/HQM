

make_sf <- function(step_size_s_grid, haz){

  surv = exp(-sapply(1:length(haz), function(i){
    sum(haz[1:i])*step_size_s_grid
  }))

  return(surv)

}
