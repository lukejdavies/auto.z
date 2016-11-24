CriteriaPeakFast = function(values){
  num <- length(values)
  out <- vector(mode = 'logical',length = num)
  
  out[2:(num-1)] = ( (values[2:(num-1)] >= values[1:(num-2)]) == 
    (values[2:(num-1)] > values[3:(num)]) ) 
  
  out[out] <- out[out] + (values[out] > values[c(out[-1],F)])
  
  return = out
}
