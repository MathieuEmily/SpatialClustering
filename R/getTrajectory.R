getTrajectory <- function(data,grid,metric="euclidean",start=-1)
{
	npts <- length(data$x)
	d <- as.matrix(dist(cbind(data$x,data$y),method=metric))
	if (start == -1){
		dtmp <- rep(-1,times=npts)
		for (i in 1:npts){
			dtmp[i] <- dBord(data$x[i],data$y[i],grid$window$xmin,grid$window$xmax,grid$window$ymin,grid$window$ymax)
			}
		cur <- which.min(dtmp)
		}
	if (start ==0){cur <- sample(1:npts,1)}
	if (start > 0){cur <- start}
	trajectory <- cur
	distance <- NULL
	#cat("i=0- cur=",cur,"\n")	
	for (i in 1:(npts-1))
	{
		wOutTraj <- (1:npts)[-trajectory]
		disttmp <- d[cur,wOutTraj]
		distance <- c(distance,min(disttmp))
		temp <- wOutTraj[which.min(disttmp)]
		cur <- temp[1]
		trajectory <- c(trajectory,cur[[1]])
	}
	return(list(trajectory=trajectory,distance=distance,metric=metric))
}
