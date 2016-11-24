MeanReject = function(data, numReject){
  if (numReject){
    # sort data
    dataValues <- sort(data)
    num <- length(dataValues)
    
    stPoint <- numReject + 1
    endPoint <- num - numReject
    
    result <- mean(dataValues[stPoint:endPoint])
  } else {
    result <- mean(data)
  }
  return = result
}
