\name{SpatialClustering}
\alias{SpatialClustering}
\title{Detection of clusters of point in a 2-dimensional space.}
\usage{
	SpatialClustering(data,window,metric="euclidean",Homogeneous=TRUE,Z=NULL,B=500,method.cut=c("gap","tree","none"),h=NULL,k=NULL)
}
\arguments{
\item{data}{A nx2 \code{\link{data.frame}}. Names of the column should be \code{x} and \code{y}}
\item{window}{A object of class \code{\link{owin}} that gives the window of study}
\item{metric}{A character vector containing the name of the distance used to compute the nearest-neighbours. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" as proposed in the \code{\link{dist}} function.}
\item{Homogeneous}{A logical flag indicating whether a covariate is used to estimate clusters}
\item{Z}{A object of \code{\link{im}} containing the value of the covariate}
\item{B}{An \code{\link{integer}} that indicates the number of Monte Carlo (“bootstrap”) samples when using the gap method for estimating the number of clusters}
\item{method.cut}{A character vector containing the name of the method used to estimate the number of clusters}
\item{h}{The height used for cutting the dendrogram. Optional}
\item{k}{The number of clusters. Optional}
}

\value{
An object of class \code{SpatClust}, i.e. a list containing the following components:
	\item{data}{A nx2 \code{\link{data.frame}}}
	\item{window}{A object of class \code{\link{owin}} that gives the window of study}
	\item{TandP}{A list with three elements. The first element is the list of trajectories, the second element is the list of pairwise probability matrix and the third element is the vector of starting points used to initialize each trajectory. More details can be found in Bar-Hen et al. (2015)}
	\item{MatDist}{A matrix of size nxn, containing the pairwiser dissimilarity measure as detailed in Bar-Hen et al. (2015)}
	\item{hh}{An object of class \code{\link{hclust}} containing the hierarchical clustering tree}
	\item{group}{A vector of size n containing the estimated group memberships}
	\item{ngroup}{The estimated number of groups}
	\item{Homogeneous}{A logical flag indicating whether a covariate is used to estimate clusters}
	\item{Z}{A object of \code{\link{im}} containing the value of the covariate. NULL if not provided.}
}
\description{
	\code{SpatialClustering} allows the detection of clusters of points in a bi-dimensional space. A covariate can be accounted for in the clustering.
}
\details{
	\code{SpatialClustering} is an implementation of the statistical procedure proposed in Bar-Hen et al. (2015). The SpatialClustering method proposes a classification of a set of bi-dimensional points given in the \code{data} object and located in the \code{window} object. Clusters are estimated by cutting a dendrogamm computed based on a dissimilarity proposed in Bar-Hen et al. (2015). The dissimilarity aims at comparing the points inter-distances to a Poisson process, that be either homogenenous (\code{Homogeneous=TRUE}) or inhomogeneous (\code{Homogeneous=FALSE}) when a covariate is accounted for. To cut the dendrogramm, two main methods are implemented: the gap statistics (\code{method="gap"} - Tibshirani, 2001) or the longest branh of the dendrogamm (\code{method="tree"}). The dendrogramm can also be cut by provided the height of cut (\code{method="none"} and \code{!is.null(h)}) of the number of desired clusters (\code{method="none"} and \code{!is.null(k)}).
	}
\examples{
## Example of a study of tree location 
data(dataExample)


## Extraction of the data and the window
dDicor <- dataExample$data
w0 <- dataExample$w0

#######
## Identification of the clusters estimated with SpatialClustering in the Homogeneous case
#######

set.seed(123)
res <- SpatialClustering(data=dDicor,window=w0)

## The group memberships
res$group

## Various plotting possibilities
plot(res)
plot(res,plot.dendro=FALSE)
plot(res,method="Seg",plot.dendro=FALSE)
plot(res,method="Seg",plot.dendro=TRUE)

#######
## Identification of the clusters estimated with SpatialClustering in the Inhomogeneous case
#######

## Extraction of the covariate
Z.Pente <- dataExample$Z.Pente

## Estimation of the cluster
set.seed(345)
res.I <- SpatialClustering(data=dDicor,window=w0,Homogeneous=FALSE,Z=Z.Pente)
plot(res.I)

}
\references{
A. Bar-Hen, M. Emily and N. Picard. (2015) Spatial Cluster Detection Using Nearest Neighbour Distance, Spatial Statistics, Vol. 14, pages 400-411.\cr
R. Tibshirani, G. Walther and T. Hastie (2001) Estimating the number of data clusters via the gap statistic. J. Roy. Stat. Soc. B 63, 411–423. \cr
}
\seealso{
\code{\link{generateListTandP}}, \code{\link{getClusters}}, \code{\link{getMatDist}}, \code{\link{plot.SpatClust}}
}

