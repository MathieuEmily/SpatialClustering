plot.convexHull<-function(data,group){
	require(tripack)
	x <- data$x
	y <- data$y
	groupf <- as.factor(group)
	for (i in 1:nlevels(groupf)){
		w <- which(groupf==levels(groupf)[i])
		if (length(w) == 2){
			segments(x0=x[w[1]],y0=y[w[1]],x1=x[w[2]],y1=y[w[2]],col=i)
		}
		if (length(w) > 2){
			tr <- tripakc::tri.mesh(x=x[w],y=y[w])
			ch <- convex.hull(tr)
			polygon(ch$x,ch$y,border=i,density=5,col=i)
		}
	}
}