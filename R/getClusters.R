getClusters <- function(MatDist,data=NULL,B=500,method=c("gap","tree","none"),h=NULL,k=NULL){
	
	method <- match.arg(method)
	if (method=="none" & is.null(h) & is.null(k)){
		stop("Either method or h or k should be provided")
	}
	hh <- hclust(as.dist(MatDist),method="single")
	if (!is.null(h)){
		group <- cutree(hh,h=h)
		ngroup <- length(table(group))
	} else{ 
		if (!is.null(k)){
			group <- cutree(hh,k=k)
			ngroup <- length(table(group))
		} else {
			if (method=="gap"){
				if (is.null(data)){stop("Argument data is missing")}
				tmpK.max <- nrow(data)-1
				tab.gap <- clusGap(x=data,FUNcluster = mycluster, K.max = tmpK.max,B=B,Mat= MatDist)
				ngroup <- maxSE(tab.gap$Tab[,3],tab.gap$Tab[,4],method="Tibs2001SEmax")
				hh <- hclust(as.dist(MatDist),method="single")
				group <- cutree(hh,k=ngroup)
			}
			if (method=="tree"){
				longuestHeight <- which.max(sapply(1:(length(hh$height)-1),FUN=function(i){hh$height[i+1]-hh$height[i]}))
				ngroup <- dim(MatDist)[1]-which.max(sapply(1:(length(hh$height)-1),FUN=function(i){hh$height[i+1]-hh$height[i]}))
				group <- cutree(hh,k=ngroup)
			}
		}
	}
	return(list(hh=hh,group=group,ngroup=ngroup))
}
