\name{getClusters}
\alias{getClusters}
\title{Estimation of clusters of points}
\usage{
	getClusters(MatDist,data=NULL,B=500,method=c("gap","tree","none"),h=NULL,k=NULL)
}

\arguments{
\item{MatDist}{A nxn dissimilarity matrix. Typically output of \code{\link{getMatDist}} function.}
\item{data}{A nx2 \code{\link{data.frame}}. Names of the column should be \code{x} and \code{y}. Argument \code{data} is used only when \code{method="gap"} is used.}
\item{B}{An \code{\link{integer}} that indicates the number of Monte Carlo (“bootstrap”) samples when using the gap method for estimating the number of clusters. Argument \code{B} is used only when \code{method="gap"} is used.}
\item{method.cut}{An string containing the name of the method used to estimate the number of clusters.}
\item{h}{The height used for cutting the dendrogram. Optional}
\item{k}{The number of clusters. Optional}
	
}

\value{
A list with three objects:
	\item{hh}{An object of class \code{\link{hclust}} containing the hierarchical clustering tree}
	\item{group}{A vector of size n containing the estimated group memberships}
	\item{ngroup}{The estimated number of groups}
}
\description{
	\code{getClusters} allows the estimation of the cluster of points obtained from dissimilarity matrix between the 2-dimensional points of the studied dataset.}
\details{
	\code{getClusters} proposes an implementation of the detection of clusters of points based on a dissimilarity matrix. In a first step, \code{getClusters} uses the dissimilarity to perform a hierarchical clustering with the single-linkage criterion. The hierarchical clustering tree is next cut to estimate the number of clusters and the cluster memberships. Cutting the tree can be performed either by using the gap statistics  (\code{method.cut="gap"} - Tibshirani et al., 2001) or by detecting the longest branch in the tree (\code{method.cut="tree"}) or by choosing an a priori height h (\code{method.cut="none"} and \code{h=h}) or by defining an a priori number of clusters k (\code{method.cut="none"} and \code{k=k}). More details can be found in Bar-Hen et al. (2015).
}
\examples{
## Example of a study of tree location 
data(dataExample)


## Extraction of the data and the window
dDicor <- dataExample$data
w0 <- dataExample$w0

## the Homogeneous case

List.Dicor.H <- generateListTandP(dDicor,w0,Homogeneous=TRUE)
MatDist.Dicor.H <- getMatDist(List.Dicor.H$Probabilities,List.Dicor.H$Trajectories) 
set.seed(123)
res.Cluster <- getClusters(MatDist=MatDist.Dicor.H,data=dDicor)

res2 <- list(data=dDicor,window=w0,TandP=List.Dicor.H,MatDist=MatDist.Dicor.H,hh=res.Cluster$hh,group=res.Cluster$group,ngroup=res.Cluster$ngroup,Homogeneous=TRUE,Z=NULL)
class(res2) <- "SpatClust"

## Equivalent to 
res <- SpatialClustering(data=dDicor,window=w0)

## the Inhomogeneous case

## Extraction of the covariate
Z.Pente <- dataExample$Z.Pente

List.Dicor.I <- generateListTandP(dDicor,w0=w0,Homogeneous=FALSE,Z=Z.Pente)
MatDist.Dicor.I <- getMatDist(List.Dicor.I$Probabilities,List.Dicor.I$Trajectories)
set.seed(345)
res.Cluster.I <- getClusters(MatDist=MatDist.Dicor.I,data=dDicor)
res2.I <- list(data=dDicor,window=w0,TandP=List.Dicor.I,MatDist=MatDist.Dicor.I,hh=res.Cluster.I$hh,group=res.Cluster.I$group,ngroup=res.Cluster.I$ngroup,Homogeneous=FALSE,Z=Z.Pente)
class(res2.I) <- "SpatClust"

## Equivalent to
set.seed(345)
res.I <- SpatialClustering(data=dDicor,window=w0,Homogeneous=FALSE,Z=Z.Pente)

}
\references{
A. Bar-Hen, M. Emily and N. Picard. (2015) Spatial Cluster Detection Using Nearest Neighbour Distance, Spatial Statistics, Vol. 14, pages 400-411.\cr
R. Tibshirani, G. Walther and T. Hastie (2001) Estimating the number of data clusters via the gap statistic. J. Roy. Stat. Soc. B 63, 411–423. \cr
}
\seealso{
\code{\link{generateListTandP}}, \code{\link{SpatialClustering}}, \code{\link{getMatDist}}
}
