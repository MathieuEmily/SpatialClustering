getWindow <- function(data,T,w0){
	n <- length(data$x)
	window <- list()
	if (T$method=="maximum"){
		for (i in 1:(n-1)){
			w <- owin(c(data$x[T$trajectory[i]]-T$distance[[i]],data$x[T$trajectory[i]]+T$distance[[i]]),c(data$y[T$trajectory[i]]-T$distance[[i]],data$y[T$trajectory[i]]+T$distance[[i]]))
			w <- intersect.owin(w0,w)
			window[[i]] <- w
			}
		}
	if (T$method=="euclidean"){
		for (i in 1:(n-1)){
			w <- spatstat::disc(radius=T$distance[i],centre=c(data$x[T$trajectory[i]],data$y[T$trajectory[i]])) ## possible conflit avec la fonction disc du package ade4
			w <- intersect.owin(w0,w)
			window[[i]] <- w
			}
		}
	return(window)
	}

