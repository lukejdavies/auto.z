NormaliseMeandev = function(flux, clipvalue, use, percentvalue = 0.95){
  
  specOut <- flux
  
  # If use keyword for indexes is not defined then 
  # set indexes to use for calculation of mean absolute deviation
  # default is to reject largest 5 percent
  if (!missing(use)) {
    cFinite <- is.finite(specOut)
    testval <- quantile( abs(specOut[which(cFinite)]), percentvalue )
    use <- which( cFinite & (abs(specOut) <= testval) )
  }

  # Iterate mean deviation clipping each time until convergence within
  # a tolerance of 0.01.
  count <- 0
  if (!missing(clipvalue)){
    testDev <- 1
    while (testDev == 1){
      meanDeviation <- mean( abs(specOut[use]) )
      specOut <- specOut / meanDeviation
      if (max(abs(specOut)) > clipvalue+0.01) {
        testDev = 1 
      } else {
        testDev = 0
      }
      specOut[which(specOut > clipvalue )] <- clipvalue
      specOut[which(specOut < -clipvalue )] <- -clipvalue
      count <- count + 1
      if (count > 10000)
        break
    }
  } else {
    meanDeviation <- mean( abs(specOut[use]) )
    specOut <- specOut / meanDeviation
  }
  
  return = specOut
}
