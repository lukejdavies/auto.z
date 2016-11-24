PolyReject = function(x, y, niterate = 15, degree = 4, highSigma=3.5, 
                         lowSigma=3.5, verbose=TRUE){ 
  # Initial setting - sigma=0 everywhere.
  sigmas <- rep(0.0, length(y))
  i <- 1
  iterationFinished <- FALSE
  
  while (!iterationFinished) {
    #weights <- rep(0,length(y)) + (sigmas < highSigma & sigmas > -lowSigma)
    #pts <- which(weights == TRUE)
    #res <- lm(y ~ poly(x, degree, raw=TRUE), weights=weights)
    
    
    pts <- sigmas < highSigma & sigmas > -lowSigma
    res <- lm(y[pts] ~ poly(x[pts], degree, raw=TRUE))
    
    
    sdUse <- sd( res$residuals )
    sigmas[pts] <- (res$residuals) / sdUse
    # test if finished iteration and print if required
    
    
    if(i == niterate) iterationFinished <- TRUE
    if( !sum( (sigmas[pts] > highSigma) | (sigmas[pts] < -lowSigma) ) ){
        if(verbose) cat('\nPOLY_REJECT: Everything within sigma bounds')
        iterationFinished <- TRUE
    }
    if(iterationFinished){
      numReject <- sum(!pts)
      if (verbose) { 
        cat('\nPOLY_REJECT: iteration=', i, '/', niterate, '  : rejected=', numReject)
      }
    }
    i <- i + 1
  }
  #reject_pts <- which(sigmas >= highSigma | sigmas <= -lowSigma)\
  #yfit <- poly_calc(res, x)
  
  return = res$coefficients
}
