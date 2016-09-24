plotCluster <- function(my.data,MatDist,ngroup=NULL,prob=NULL,w0=owin(c(0,250),c(0,250))){
	clust <- getClusters(MatDist,ngroup=ngroup,prob=prob) ## Calcul des clusters à partir du lien minimal dans la classification hiérarchique
	hh <- clust$hh ## Récupération de la classification hiérarchique
	group <- clust$group ## Récupération des groupes
	height <- getHeightCut(hh,ngr=clust$ngroup) ## Stockage de la hauteur de coupe dans l'arbre de classification

	par(mfrow=c(1,2))
	plot(hh,hang=-1)
	abline(h=height,col=2,lty=2)
	plot(my.data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]),main="Homogeneous")
	plot.segments(mat=MatDist,thresh=height,data=my.data)
}


plotCluster <- function(x){
	clust <- getClusters(MatDist,ngroup=ngroup,prob=prob) ## Calcul des clusters à partir du lien minimal dans la classification hiérarchique
	hh <- clust$hh ## Récupération de la classification hiérarchique
	group <- clust$group ## Récupération des groupes
	height <- getHeightCut(hh,ngr=clust$ngroup) ## Stockage de la hauteur de coupe dans l'arbre de classification

	par(mfrow=c(1,2))
	plot(hh,hang=-1)
	abline(h=height,col=2,lty=2)
	plot(my.data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]),main="Homogeneous")
	plot.segments(mat=MatDist,thresh=height,data=my.data)
}
