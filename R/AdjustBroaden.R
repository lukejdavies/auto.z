AdjustBroaden = function(spec) {
  specOut <- spec
  length <- length(specOut)
  max <- max(specOut)
  index <- which(specOut == max)

  specOut[index-1] <- max #TODO may set specOut[0] to max.. is this okay?
  specOut[index+1] <- max
  # above may add an extra piece to the array, below will ignore it
  return = specOut[1:length]
}
