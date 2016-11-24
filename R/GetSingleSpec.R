GetSingleSpec = function(file = "../autozResouces/SG23_Y7_003.fits", colmn=1, logw=F, sc=1, xunit='ang', 
                         yunit='ang', z=NA, verbose = TRUE) { 
  #Read in first level of file
  specf<-readFITS(file, hdu = 1)  
  # Attempt to read information from header
  if(missing(z) && !(z <- GetHeaderValue(specf$hdr, 'Z')) )
    cat('\nGetSingleSpec(): Can\'t find z in header and not supplied')
  
  if ( !(UTMJD <- GetHeaderValue(specf$hdr, 'UTMJD')) )
    cat('\nGetSingleSpec(): Can\'t find UTMJD in header')
  
  if ( !(latitude <- GetHeaderValue(specf$hdr, 'LAT_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find latitude in header')
  
  if ( !(longitude <- GetHeaderValue(specf$hdr, 'LONG_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find longitude in header')
  
  if ( !(altitude <- GetHeaderValue(specf$hdr, 'ALT_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find altitude in header')
  
  if (length(dim(specf$imDat))>0) 
    flux <- specf$imDat[,colmn]*sc
  
  if (length(dim(specf$imDat))==0) 
    flux <- specf$imDat*sc
  
  lambda <- specf$axDat$crval[1] + (c(1:(specf$axDat$len[1]) - specf$axDat$crpix[1]  ) * specf$axDat$cdelt[1])
  
  # Read in second line of file, extract variance
  specf2    <- readFITS(file, hdu = 2)
  if (length(dim(specf2$imDat))>0) {
    variance <- specf2$imDat[,colmn]*sc
  }
  if (length(dim(specf2$imDat))==0) {
    variance <- specf2$imDat*sc
  }
  
  # Read in third line of file, extract name, RA, DEC, comment. Also pin lat/long info to structure
  specf3    <- readFITS(file, hdu = 3)
  info      <- c(specf3$col[[1]][colmn], specf3$col[[2]][colmn], specf3$col[[3]][colmn], 
                 specf3$col[[13]][colmn], UTMJD, latitude, longitude, altitude)
  infoNames <- c(specf3$colNames[c(1:3,13)], 'UTMJD', 'latitude', 'longitude', 'altitude')
  
  if (logw==T) {lambda<-10.^lambda}  
  spec <- list(lambda, flux, xunit, yunit, z, sqrt(variance), info[1], as.numeric(info[2]), as.numeric(info[3]), 
               info[4], as.numeric(info[5]), as.numeric(info[6]), as.numeric(info[7]), as.numeric(info[8]))
  names(spec) <- c('lambda', 'flux', 'xunit', 'yunit', 'z', 'error', infoNames)
  return = spec      
}

