GetAllSpec = function(file = "../autozResouces/SG23_Y7_003.fits", logw=F, sc=1, xunit='ang', yunit='ang', 
                      z=NA, verbose = TRUE) { 
  #Read in first three levels of file
  specf     <- readFITS(file, hdu = 1)  #contains flux data
  specf2    <- readFITS(file, hdu = 2)  #contains varience
  specf3    <- readFITS(file, hdu = 3)  #contains misc data eg RA DEC
  
  # Attempt to read information from header
  if(missing(z) && !(z <- GetHeaderValue(specf$hdr, 'Z')) )
    cat('\nGetAllSpec(): Can\'t find z in header and not supplied')
  
  if ( !(UTMJD <- GetHeaderValue(specf$hdr, 'UTMJD')) )
    cat('\nGetAllSpec(): Can\'t find UTMJD in header')
  
  if ( !(latitude <- GetHeaderValue(specf$hdr, 'LAT_OBS')) )
    cat('\nGetAllSpec(): Can\'t find latitude in header')
  
  if ( !(longitude <- GetHeaderValue(specf$hdr, 'LONG_OBS')) )
    cat('\nGetAllSpec(): Can\'t find longitude in header')
  
  if ( !(altitude <- GetHeaderValue(specf$hdr, 'ALT_OBS')) )
    cat('\nGetAllSpec(): Can\'t find altitude in header')
  
  lambda <- specf$axDat$crval[1] + (c(1:(specf$axDat$len[1]) - specf$axDat$crpix[1]  ) * specf$axDat$cdelt[1])
  if (logw==T) 
    lambda <- 10.^lambda
  
  #load in spectrums (fluxes)
  dimensions <- dim(specf$imDat)
  # Select target fibres only. 
  indexUse <- which(specf3$col[[9]] == 'P')
  specAll <- vector("list", dimensions[2])
  # TODO not sure if all files will have the same dimension format .. ? test with other files
  # note the below for loops checks the name of a spectrum, if it starts with 'G' it should be used
  for(colmn in indexUse){
    flux <- specf$imDat[,colmn]*sc
    variance <- specf2$imDat[,colmn]*sc
    
    # Using third line of file, extract name, RA, DEC, comment. Also pin lat/long info to structure
    info      <- c(specf3$col[[1]][colmn], specf3$col[[2]][colmn], specf3$col[[3]][colmn], 
                   specf3$col[[13]][colmn], UTMJD, latitude, longitude, altitude)
    infoNames <- c(specf3$colNames[c(1:3,13)], 'UTMJD', 'latitude', 'longitude', 'altitude')
    
    spec <- list(flux, xunit, yunit, z, suppressWarnings(sqrt(variance)), info[1], as.numeric(info[2]), as.numeric(info[3]), 
                 info[4], as.numeric(info[5]), as.numeric(info[6]), as.numeric(info[7]), as.numeric(info[8]))
    names(spec) <- c('flux', 'xunit', 'yunit', 'z', 'error', infoNames)
    specAll[[colmn]] <- spec  
  }
  
  specAll$lambda <- lambda
  specAll$indexUse <- indexUse
  return = specAll  
}
