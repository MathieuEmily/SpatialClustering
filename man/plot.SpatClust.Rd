\name{plot.SpatClust}
\alias{plot.SpatClust}
\title{Plot a set of two-dimensional points with a cluster membership.}
\usage{
\method{plot}{SpatClust}(x,method=c("CH","Seg"),plot.dendro=TRUE,plot.covariate=FALSE)
}
\arguments{
\item{x}{\code{SpatClust}. Given as output of \code{\link{SpatialClustering}}}
\item{method}{A character string indicating the type of cluster visualization}
\item{plot.dendro}{A boolean. Should the dendrogram be displayed ? (See Details for more information)}
\item{plot.covariate}{A boolean. Should the map of the covariate be displayed ? (See Details for more information)}
}

\value{
The form of the value returned by \code{plot} depends on the class of its argument. See Details.}
\description{
	\code{plot.SpatClust} is a graphical function that the visualization of the clusters.}
\details{
	If \code{method="CH"}, this function draws the convex hull of each cluster. If \code{method="Seg"}, \code{plot.SpatClust} draws an edge between all pairs of points that are connected (i.e. closer than expected under the underlying homogeneous or inhomogeneous Poisson process).
	If \code{plot.dendro=TRUE}, the graphical output is divided into two parts: one part with the dendrogram and one part with two-dimensional clusters. If \code{plot.dendro=FALSE}, two-dimensional clusters are only displayed.
	If \code{plot.covariate=TRUE}, the two-dimensional clusters are accompanied with a image of the covariate on the window of interest. 
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
\code{\link{generateListTandP}}, \code{\link{getClusters}}, \code{\link{getMatDist}}, \code{\link{SpatialClustering}}
}

