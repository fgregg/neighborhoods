library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

# Topology of Block Connectivity
blocks <-spdep::poly2nb(block.groups.poly,
                 foundInBox=rgeos::gUnarySTRtreeQuery(block.groups.poly))
edgelist <- common::nb2edgelist(blocks)
write.csv(edgelist, file="../interchange/edges.csv", row.names=FALSE)

centroids <- coordinates(block.groups.poly)

edge.weights <- rep(1, dim(edgelist)[1])


railroad.intersects <- common::edgesIntersect(edgelist,
                                              centroids,
                                              railroads,
                                              projection)

highway.intersects <- common::edgesIntersect(edgelist,
                                             centroids,
                                             highways,
                                             projection)
  
grid.street.intersects <- common::edgesIntersect(edgelist,
                                                 centroids,
                                                 grid.streets,
                                                 projection)

water.intersects <- common::edgesIntersect(edgelist,
                                           centroids,
                                           water,
                                           projection)



rail.weights <- as.numeric(railroad.intersects)
write.csv(rail.weights, file="../interchange/rail_intersects.csv", row.names=FALSE)
rail.weights <- (rail.weights - 1) * -1

highway.weights <- as.numeric(highway.intersects)
write.csv(highway.weights, file="../interchange/highway_intersects.csv", row.names=FALSE)
highway.weights <- (highway.weights - 1) * -1

water.weights <- as.numeric(water.intersects)
write.csv(water.weights, file="../interchange/water_intersects.csv", row.names=FALSE)
water.weights <- (water.weights - 1) * -1

grid.street.weights <- as.numeric(grid.street.intersects)
write.csv(grid.street.weights, file="../interchange/grid_intersects.csv", row.names=FALSE)
grid.street.weights <- (grid.street.weights -1) * -1

edge.weights = grid.street.weights * rail.weights * highway.weights * water.weights

write.csv(edge.weights, file="edge_weights.csv", row.names=FALSE)
