CriteriaBetween = function(data, data_range){
  criteria <- (data > data_range[1]) & (data <= data_range[2])
  return = criteria
}
