generateListTandP <- function(data,w0=owin(c(0,250),c(0,250)),Homogeneous=TRUE,Z=NULL,myPPM=NULL,start.points=NULL){
	npts <- nrow(data) ## number of points in the point process
	if (is.null(start.points)){
		start.points <- 1:npts
	}
	else {
		npts <- length(start.points)
	}
	if (!Homogeneous & is.null(Z)){
			stop("Covariate is missing for the inhomogeneous case")
		}
	grid <- MakeGrid(xmin=w0$xrange[1],xmax=w0$xrange[2],ymin=w0$yrange[1],ymax=w0$yrange[2])
	Trajectories <- list() ## Initialisation
	Probabilities <- list() ## Initialisation

	for (k in 1:length(start.points)){
		i <- start.points[k]
		cat("Start",k,"sur",length(start.points),": numero du point",i,"\n")
		tmp <- getTrajectory(data,grid,start=i)
		Trajectories[[k]] <- tmp
		wtmp <- getWindow(data,tmp,w0)
		ptmp <- getProb(data=data,window=wtmp,w0=w0,Homogeneous=Homogeneous,Z=Z,myPPM=myPPM)
		Probabilities[[k]] <- ptmp
		}
	return(list(Trajectories=Trajectories,Probabilities=Probabilities,start.points=start.points))
}
