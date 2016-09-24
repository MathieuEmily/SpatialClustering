getProb <- function(data,window,w0,Homogeneous=TRUE,Z=NULL,intensity=NULL,myPPM=NULL,ngrid=100){
	wincur <- window[[1]]
	n <- length(data$x)
	if (Homogeneous){ ## processus homogene
		if (is.null(intensity)){
			intensity <- n/area.owin(w0)
		}	
	}
	else {
		if (is.null(myPPM)){
			myPPP <- ppp(data$x,data$y,window=w0) ## Point process without covariate
			if (is.null(Z)){break("Covariate is missing")}## Inhomogeneous Case - Covariate is needed for weighting intensity
			else{
				#xx <- seq(Z$xrange[1],Z$xrange[2],length=Z$dim[1]) ## x-Location of 
				#yy <- seq(Z$yrange[1],Z$yrange[2],length=Z$dim[2]) ## x-Location of
				myPPM <- ppm(myPPP, ~ Z, covariates=list(Z=Z)) ## Estimation of the point process with covariate
			}
		}
	} 
	
	
	probObs <- rep(-1,times=(n-1)) ## Initialization of the vector
	
	## First probability
	if (Homogeneous){
		probObs[1] <- 1-exp(-intensity*area.owin(wincur))
	}
	else{
		# Calculation of the mean intensity according to the covariate	
		tmp.predict <- predict(myPPM,window=wincur,ngrid=ngrid)
		lambdatmp <- mean(tmp.predict,na.rm=TRUE)
		probObs[1] <- 1-exp(-lambdatmp*area.owin(wincur))
	}		
	## Next probabilities
	if (n > 2){
		for (i in 2:(n-1)){
			wincur <- window[[i]]
			if (Homogeneous){ 
				## "Homogeneous"
				tmp.win <- wincur
				for (j in 1:(i-1)){
					tmp.win <- setminus.owin(tmp.win,window[[j]])
				} 
				probObs[i] <- 1-exp(-intensity*(area.owin(tmp.win)))
			}
			else { 
				## "Inhomogeneous"
				# Calculation of the window
				tmp.win <- wincur
				for (j in 1:(i-1)){
					tmp.win <- setminus.owin(tmp.win,window[[j]])
				}
				# Calculation of the mean intensity according to the covariate				
				tmp.predict <- predict(myPPM,window=tmp.win,ngrid=ngrid)
				lambdatmp <- mean(tmp.predict,na.rm=TRUE)
				probObs[i] <- 1-exp(-lambdatmp*area.owin(tmp.win))
			}
		}
	}
	return(probObs)
}

