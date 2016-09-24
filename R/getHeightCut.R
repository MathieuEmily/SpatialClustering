getHeightCut <- function(hh,ngr){
	npts <- length(hh$height)
	res <- hh$height[npts-ngr+1]+(hh$height[npts-ngr+2]-hh$height[npts-ngr+1])/2
	return(res)
}
