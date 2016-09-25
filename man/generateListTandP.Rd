\name{generateListTandP}
\alias{generateListTandP}
\title{Computation of the 1-dimensional trajectories and of the nearest-neighbour probabilities}
\usage{
	generateListTandP(data,w0=owin(c(0,250),c(0,250)),Homogeneous=TRUE,Z=NULL,myPPM=NULL)
}

\arguments{
\item{data}{A nx2 \code{\link{data.frame}}. Names of the column should be \code{x} and \code{y}}
\item{w0}{A object of class \code{\link{owin}} that gives the window of study}
\item{Homogeneous}{A logical flag indicating whether a covariate is used to estimate clusters}
\item{Z}{A object of \code{\link{im}} containing the value of the covariate. Optional.}
\item{myPPM}{An object of class \code{\link{ppm}}. Optional}
}

\value{
An list with 3 objects \describe{
	\item{Trajectories}{A list of n trajectories where a trajectory is a list of three elemets: \code{trajectory} a vector of size n with the sequence of points included in the 1-dimensional path, \code{distance} a vector of size n-1 with the sequence of interdistances between consecutive points and \code{method} the distance used in the trajectory.}
	\item{Probabilities}{A list of n vector of size n-1 where each vector contains the sequence of interdistance probabilities.}
	\item{start.points}{A vector of size n containing the starting point used for each trajectory.}
}
\description{
	\code{generateListTandP} allows the computation of the 1-dimensional trajectories and of the nearest-neighbour probabilities. Probabilities are computed based on a Poisson reference point process.
	}
\details{
	\code{generateListTandP} proposes the computation of the n 1-dimensional trajectories where each trajectory draw a path along all bi-dimensional points of the original data. Trajectories differs by the starting point used to initialize the path. To go from one point to another point in the trajectory, we used the nearest-neighbour point that has not yet been included in the trajectory. Therefore, each trajectory is made by a sequence of exactly n distinct points. All trajectories are stored in the output object \code{Trajectories}. For each trajectory, the interdistance is used to compute a probability of clustering between consecutive points. Such a probability refers to a likelihood of the observed interdistance under a Poisson process. The underlying Poisson process can be either Homogeneous or Inhomogeneous if a covariate is accounted for. The list of probability matrices is stored in the output object \code{Probabilities}. 
	}
\examples{
## Example of a study of tree location 
data(dataExample)


## Extraction of the data and the window
dDicor <- dataExample$data
w0 <- dataExample$w0

## the Homogeneous case

List.Dicor.H <- generateListTandP(dDicor,w0,Homogeneous=TRUE)

## the Inhomogeneous case

## Extraction of the covariate
Z.Pente <- dataExample$Z.Pente

List.Dicor.I <- generateListTandP(dDicor,w0=w0,Homogeneous=FALSE,Z=Z.Pente)

}
\references{
A. Bar-Hen, M. Emily and N. Picard. (2015) Spatial Cluster Detection Using Nearest Neighbour Distance, Spatial Statistics, Vol. 14, pages 400-411.\cr
}
