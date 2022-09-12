dataset_split <- function(I, data){
  n = length(unique(as.numeric(data$id)))
  data_list = list()
  for(i in 1:floor(n/I)){
    data_list[[i]] = data[!(data$id %in% (1:I +(i-1)*I)),]
  }
  if(floor(n/I) < (n/I)){data_list[[i+1]] = data[!(data$id %in% (1:I +(i)*I)),]}

  return(data_list)
}
