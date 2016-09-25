# SpatialClustering

To install and load the package in R

```ruby
library(devtools)
install_github("MathieuEmily/SpatialClustering")
library(SpatialClustering)
```

Example of a study of tree location 
```ruby
data(dataExample)
```

Extraction of the data and the window
```ruby
dDicor <- dataExample$data
w0 <- dataExample$w0
```

### Identification of the clusters estimated with SpatialClustering in the Homogeneous case

```ruby
set.seed(123)
res <- SpatialClustering(data=dDicor,window=w0)
```

The group memberships
```ruby
res$group
```

Various plotting possibilities
```ruby
plot(res)
plot(res,plot.dendro=FALSE)
plot(res,method="Seg",plot.dendro=FALSE)
plot(res,method="Seg",plot.dendro=TRUE)
```

### Identification of the clusters estimated with SpatialClustering in the Inhomogeneous case
Extraction of the covariate
```ruby
Z.Pente <- dataExample$Z.Pente
```

Estimation of the cluster
```ruby
set.seed(345)
res.I <- SpatialClustering(data=dDicor,window=w0,Homogeneous=FALSE,Z=Z.Pente)
plot(res.I)
```