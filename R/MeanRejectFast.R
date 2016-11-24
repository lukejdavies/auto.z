MeanRejectFast = function(data, numReject){
  tooLarge <- PriorityQueue(decreasing = F)
  tooSmall <- PriorityQueue(decreasing = T)
  for(i in 1:numReject){
    tooLarge[['insert']](data[i], i)
    tooSmall[['insert']](data[i], i)
  }
  
  for(i in (numReject+1):length(data)){
    if(data[i] > tooLarge[['peak']]()[1]){
      tooLarge[['insert']](data[i], i)
      tooLarge[['pop']]()
    }
    if(data[i] < tooSmall[['peak']]()[1]){
      tooSmall[['insert']](data[i], i)
      tooSmall[['pop']]()
    }
  }
  rejectIndex = c(tooSmall[['dump']]()[[2]], tooLarge[['dump']]()[[2]])
  newData = data[-rejectIndex]
  result = mean(newData)
  return = newData
}
