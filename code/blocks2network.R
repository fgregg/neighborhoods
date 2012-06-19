library(maptools)
library(rgdal)
library(spdep)
library(rgeos)
library(igraph)

nb2edgelist <- function(nb) {
  el <- c()
  for (i in 1:length(nb)) {
    neighbors <- blocks[[i]]
    new.neighbors <- neighbors[neighbors > i]
    if (length(new.neighbors) > 0) {
      el <- rbind(el, cbind(i, new.neighbors))
    }
  }
  return(el)
}

# Projection and Bounding Box Range

projection = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
range = cbind(c(-87.7, -87.6), c(41.89,41.98))
range = project(range, projection)


blocks.poly <- readOGR("../admin_areas/CensusBlockTIGER2010.shp", "CensusBlockTIGER2010")

blocks.poly <- blocks.poly[coordinates(blocks.poly)[,1] > range[1,1] &
                           coordinates(blocks.poly)[,1] < range[2,1] &
                           coordinates(blocks.poly)[,2] > range[1,2] &
                           coordinates(blocks.poly)[,2] < range[2,2],]

centroids <- coordinates(blocks.poly)

blocks <-poly2nb(blocks.poly,
                 foundInBox=gUnarySTRtreeQuery(blocks.poly))



blocks <- nb2edgelist(blocks)
blocks <- graph.edgelist(blocks, directed=FALSE)


