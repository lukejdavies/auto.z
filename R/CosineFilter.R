CosineFilter = function(num, reverse=FALSE){
  if(num<2){
    return = 0
  }
  phase <- (0:(num-1))  /(num-1) * pi
  if (!reverse){
    filter <- 0.5 * (1.0 - cos(phase)) 
  } else { 
    filter <- 0.5 * (1.0 + cos(phase))
  }
  return = filter
}
