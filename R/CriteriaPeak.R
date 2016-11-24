CriteriaPeak = function(values){
  num <- length(values)
  out <- rep(FALSE, num)
  out[2:(num-3)] =  (values[2:(num-3)] >= values[1:(num-4)]) & 
                    (values[2:(num-3)] > values[3:(num-2)])
  return = out
}
