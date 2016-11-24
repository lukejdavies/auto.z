MedianAdjust = function(x, width){
  
  num <- length(x) # example num=1000
  outX <- runmed(x, width, endrule = 'keep')[1:length(x)]   # example width=51
  
  # use a filter of half the width for the edge points
  i1 <- (width %/% 2 - 1)   # example i1=24
  if(i1<1) i1 <- 0
  i2 <- width %/% 2             # example i2=25
  
  # Replace edge points not covered by idl median.pro (runmed in R doesn't have an endrule consistent with this code)
  # example 0:24    becomes median of 0:25
  outX[1:(i1+1)]     <- median(x[1:(i2+1)])   
  # example 975:999 becomes median of 974:999
  outX[(num-i1):num] <- median(x[(num-i2):num])
  
  return = outX

}
