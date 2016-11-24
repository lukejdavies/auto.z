MedSmooth = function(spec, medWidth = 51, smoothWidth = 121) {
  # Smooth with median filter. 
  outSpec <- MedianAdjust(spec,medWidth)
  
  # Further smooth using standard boxcar smoothing. 
  outSpec <- RunningMeanSmooth(x = outSpec, width = smoothWidth)
  
  return = outSpec
}
