Autoz.Single.Spec = function(spec, plan, verbose = TRUE,
                           oversample = 5, num = 5, templateNumbers = c(2:14, 16:22,40:47),
                           useInvCorrection = TRUE, stLambda = 4000, endLambda = 9000, 
                           minval = -1.0e4, maxval = 1.0e6, z_prior=c(-1,1000), UTMJD=56835.65, longitude=0,latitude=0,altitude=0,RA=0,DEC=0){
  
  Tflux<-spec$flux[which(is.finite(spec$flux)==T & is.finite(spec$wave)==T)]
  Twave<-spec$wave[which(is.finite(spec$flux)==T & is.finite(spec$wave)==T)]
  Terror<-spec$error[which(is.finite(spec$flux)==T & is.finite(spec$wave)==T)]
  spec$flux<-Tflux
  spec$wave<-Twave
  spec$error<-Terror
  spec$UTMJD<-UTMJD
  spec$longitude<-longitude
  spec$latitude<-latitude
  spec$altitude<-altitude
  spec$RA<- RA
  spec$DEC<- DEC
  spec$lambda <- spec$wave
  
  #set up new lambda scale to rebin spectrum and templates to
  logLambdaData <- SetUpLogLambda(verbose = verbose, oversample = oversample)
  newLogLambda <- logLambdaData$logLambda
  
  spec <- ProcessSpectrum(spec, stLambda = stLambda, endLambda = endLambda, minval = minval, maxval = maxval, 
                          useInvCorrection = useInvCorrection,  verbose = verbose)

  
  spec$countHighval  <- length(which(spec$flux > 20))
  spec$countLowval   <- length(which(spec$flux < -20))
  spec$meanadNorm    <- mean(abs(spec$flux))
  spec$rmsNorm       <- sqrt( mean(spec$flux^2) )
  
  # Convert spectral lambda to vacuum wavelength from air wavelength input.
  logVlambda <- log(VacuumFromAir(spec$lambda),10)  

  specRebin <- approx(x = c(3.0, logVlambda[1] - 0.001, logVlambda, logVlambda[length(logVlambda)]+0.001, 4.5), 
                      y = c(0, 0, spec$flux, 0, 0),
                      xout = newLogLambda, method = "linear", yleft=0.0, yright=0.0)#TODO check interpol is correct
  spec$lambda <- specRebin$x
  spec$flux <- specRebin$y
  
  tempData <- RebinTempData(newLogLambda, templateNumbers=templateNumbers, verbose = verbose)
  
  
  helioVel <- Heliocentric(spec$RA*180/pi, spec$DEC*180/pi, 2000, jd = spec$UTMJD, longitude = spec$longitude, 
                          latitude = spec$latitude, altitude = spec$altitude)

  #if(missing(plan)) plan = planFFT(length(spec$flux), 0)
  if(missing(plan)) plan = 0
  
  
  ccinfo <- DoCrossCorr(spec = spec, gap = logLambdaData$gap, tempData = tempData, helioVel = helioVel, plan = plan, z_prior=z_prior)

  peaks <- FindHighestPeaks(ccinfo, num=num)

  spec$ccSigma <- rep(-1,length(peaks))
  i <- 0
  
  for( peak in peaks ){
      
    peak$redshift   <- FindMax(xArray = ccinfo[[peak$templateID]]$shifts[(peak$shiftIndex-3):(peak$shiftIndex+3)],
                               yArray = ccinfo[[peak$templateID]]$crossCorrRaw[(peak$shiftIndex-3):(peak$shiftIndex+3)],
                               n = 2)
    spec$ccSigma[i <- i+1] <- peak$crossCorr
  }
  
  spec <- CalculateCertainty(spec)

  spec$best$z <- peaks[[1]]$redshift
  spec$best$temp <- peaks[[1]]$template
  spec$best$prob <- spec$prob
  
  #print results
  if(verbose){
    cat('\n***AUTOZ Outputs***\n')
    cat("Template\tRedshift\t\tCrossCorr\t\tShiftIndex\n")
    for( peak in peaks ){
      cat(peak$template, peak$redshift, peak$crossCorr, peak$shiftIndex,"\n", sep="\t\t")
    }
    cat("Probabilty on best match is ",spec$prob,"\n")
  }
  
  spec$results        <- c(peaks[[1]]$redshift, peaks[[1]]$crossCorr, peaks[[1]]$template, peaks[[2]]$redshift, peaks[[2]]$crossCorr, peaks[[2]]$template, peaks[[3]]$redshift, peaks[[3]]$crossCorr, peaks[[3]]$template, peaks[[4]]$redshift, peaks[[4]]$crossCorr, peaks[[4]]$template)
  names(spec$results) <- c('Z','CC_SIGMA','TEMPLATE','Z2','CC_SIGMA2','TEMPLATE2','Z3','CC_SIGMA3','TEMPLATE3', 'Z4','CC_SIGMA4','TEMPLATE4')
  
  return = spec
}
