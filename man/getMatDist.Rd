\name{getMatDist}
\alias{getMatDist}
\title{Computation of a dissimilarity matrix}
\usage{
	getMatDist(Proba,Traj)
}

\arguments{
\item{Proba}{A list of n matrices of pairwise probabilities (each of size (n-1)x(n-1)). Typically the \code{Probabilities} object obtain from \code{\link{generateListTandP}}.}
\item{Traj}{A list of n list of trajectories. Typically the \code{Trajectories} object obtain from \code{\link{generateListTandP}}.}
}

\value{
An nxn matrix containing all pairwise dissimilarity between the n points of the dataset.}
}
\description{
	\code{getMatDist} allows the dissimilarity matrix between the 2-dimensional points of the studied dataset.}
\details{
	\code{getMatDist} proposes the implementation of the minimum of all pairwise probabilities. It relates to the single-linkage criterion  in hierarchical clustering. More details are given in Bar-Hen et al. (2015).
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

## the Inhomogeneous case

## Extraction of the covariate
Z.Pente <- dataExample$Z.Pente

List.Dicor.I <- generateListTandP(dDicor,w0=w0,Homogeneous=FALSE,Z=Z.Pente)
MatDist.Dicor.I <- getMatDist(List.Dicor.I$Probabilities,List.Dicor.I$Trajectories)

}
\references{
A. Bar-Hen, M. Emily and N. Picard. (2015) Spatial Cluster Detection Using Nearest Neighbour Distance, Spatial Statistics, Vol. 14, pages 400-411.\cr
}
