SpatialClustering <- function(data,window,Homogeneous=TRUE,Z=NULL,B=500,method.cut=c("gap","tree","none"),h=NULL,k=NULL){
	method.cut <- match.arg(method)
	TandP <- generateListTandP(data=data,w0=window,Homogeneous=Homogeneous,Z=Z)
	MatDist <- getMatDist(TandP$Probabilities,TandP$Trajectories)
	clust <- getClusters(MatDist=MatDist,data=data,B=B,method=method.cut,h=h,k=k)
	res <- list(data=data,window=window,TandP=TandP,MatDist=MatDist,hh=clust$hh,group=clust$group,ngroup=clust$ngroup,Homogeneous=Homogeneous,Z=Z)
	class(res) <- "SpatClust"
	return(res)
}
