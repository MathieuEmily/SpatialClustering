myf <- function(P,Traj){
	npts <- length(P)+1
	M <- matrix(0,ncol=npts,nrow=npts)
	for (i in 1:(npts-1)){
		for (j in (i+1):(npts)){
			w1 <- which(Traj$trajectory==i)
			w2 <- which(Traj$trajectory==j)
			#cat(i,j,w1,w2,"\n")
			if (w1 < w2){
				M[i,j] <- max(P[w1:(w2-1)])
				M[j,i] <- M[i,j]
			}
			else{
				M[i,j] <- max(P[w2:(w1-1)])
				M[j,i] <- M[i,j]
			}
		}
	}
	return(M)
}
