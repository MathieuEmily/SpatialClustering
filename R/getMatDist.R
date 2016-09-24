getMatDist <- function(Proba,Traj){
	npts <- length(Proba)
	resM <- myf(Proba[[1]],Traj[[1]])
	for (k in 2:npts){
		cat(k,"sur",npts,"\n")
		Mtmp <- myf(Proba[[k]],Traj[[k]])
		resM <- pmin(resM,Mtmp)
		}
	return(resM)
}
