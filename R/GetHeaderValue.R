GetHeaderValue = function(header, valueName){
  if (length(as.numeric(header[which(header==valueName)+1])) > 0) {
    return = as.numeric(header[which(header==valueName)+1])
  } else {
    return = FALSE
  }
}

