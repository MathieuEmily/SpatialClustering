MakeGrid <- function(xmin=0,xmax=1,ymin=0,ymax=1,nx=10,ny=10)
{
	x_by <- (xmax-xmin)/nx
	val_x <- seq(xmin+x_by/2,xmax-x_by/2,by=x_by)
	y_by <- (ymax-ymin)/ny
	val_y <- seq(ymin+y_by/2,ymax-y_by/2,by=y_by)
	x <- rep(val_x,times=ny)
	y <- rep(-1,times=nx*ny)
	for (i in 1:ny){y[(nx*(i-1)+1):(nx*i)] <- rep(val_y[i],times=nx)}
	res <- list(coordinates=data.frame(x=x,y=y,include=rep(1,times=nx*ny)),window=list(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),by=list(xby=x_by,yby=y_by))
	return(res)
}
