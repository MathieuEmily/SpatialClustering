plot.segments<-function(mat,thresh,data){
	n <- ncol(mat)
	for (i in 1:(n-1)){
		for (j in (i+1):n){
			if (mat[i,j] <= thresh){
				segments(x0=data$x[i],y0=data$y[i],x1=data$x[j],y1=data$y[j])
			}
		}
	} 
}
