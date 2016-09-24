mycluster <- function(x, k,Mat){
	res <- list()
	res$cluster <- cutree(hclust(as.dist(Mat), method = "single"),k=k)
	return(res)
	}
