plot.SpatClust <- function(x,method=c("CH","Seg"),plot.dendro=TRUE,plot.covariate=FALSE){
	method <- match.arg(method)
	w0 <- x$window
	hh <- x$hh ## Récupération de la classification hiérarchique
	group <- x$group ## Récupération des groupes
	height <- getHeightCut(hh,ngr=x$ngroup) ## Stockage de la hauteur de coupe dans l'arbre de classification

	if (method=="CH"){
		group <- x$group
		xx <- x$data$x
		yy <- x$data$y
		groupf <- as.factor(group)
		if (plot.dendro){
			par(mfrow=c(1,2))
			plot(hh,hang=-1,xlab="",ylab="Dissimilarity")
			abline(h=height,col=2,lty=2)
			if (plot.covariate){
				plot(x$Z)
				points(x$data,col=group,pch=19)
			} else{
				plot(x$data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]))
			}
			for (i in 1:nlevels(groupf)){
				w <- which(groupf==levels(groupf)[i])
				if (length(w) == 2){
				segments(x0=xx[w[1]],y0=yy[w[1]],x1=xx[w[2]],y1=yy[w[2]],col=i)
				}
				if (length(w) > 2){
					tr <- tripack::tri.mesh(x=xx[w],y=yy[w])
					ch <- tripack::convex.hull(tr)
					polygon(ch$x,ch$y,border=i,density=5,col=i)
				}
			}
		} else {
			if (plot.covariate){
				plot(x$Z)
				points(x$data,col=group,pch=19)
			} else{
				plot(x$data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]))
			}
			for (i in 1:nlevels(groupf)){
				w <- which(groupf==levels(groupf)[i])
				if (length(w) == 2){
				segments(x0=xx[w[1]],y0=yy[w[1]],x1=xx[w[2]],y1=yy[w[2]],col=i)
				}
				if (length(w) > 2){
					tr <- tripack::tri.mesh(x=xx[w],y=yy[w])
					ch <- tripack::convex.hull(tr)
					polygon(ch$x,ch$y,border=i,density=5,col=i)
				}
			}
		}
	}
	if (method=="Seg"){
		if (plot.dendro){
			par(mfrow=c(1,2))
			plot(hh,hang=-1,xlab="",ylab="Dissimilarity")
			abline(h=height,col=2,lty=2)
			if (plot.covariate){
				plot(x$Z)
				points(x$data,col=group,pch=19)
			} else{
				plot(x$data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]))
			}
			plot.segments(mat=x$MatDist,thresh=height,data=x$data)
		} else {
			if (plot.covariate){
				plot(x$Z)
				points(x$data,col=group,pch=19)
			} else{
				plot(x$data,col=group,pch=19,xlim=c(w0$xrange[1],w0$xrange[2]),ylim=c(w0$yrange[1],w0$yrange[2]))
			}
			plot.segments(mat=x$MatDist,thresh=height,data=x$data)
		}
	}
}