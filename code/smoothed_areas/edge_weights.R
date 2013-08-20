library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

# Topology of Block Connectivity
blocks <-spdep::poly2nb(blocks.poly,
                 foundInBox=rgeos::gUnarySTRtreeQuery(blocks.poly))
blocks <- common::nb2edgelist(blocks)
write.csv(blocks, file="../interchange/edges.csv", row.names=FALSE)


edge.weights <- rep(1, dim(blocks)[1])

rail.weights <- as.numeric(railroad.intersects)
write.csv(rail.weights, file="../interchange/rail_intersects.csv", row.names=FALSE)
rail.weights <- (rail.weights - 1) * -1

highway.weights <- as.numeric(highway.intersects)
write.csv(highway.weights, file="../interchange/highway_intersects.csv", row.names=FALSE)
highway.weights <- (highway.weights - 1) * -1

water.weights <- as.numeric(water.intersects)
write.csv(water.weights, file="../water_intersects.csv", row.names=FALSE)
water.weights <- (water.weights - 1) * -1

grid.street.weights <- as.numeric(grid.street.intersects)
write.csv(grid.street.weights, file="../interchange/grid_intersects.csv", row.names=FALSE)
grid.street.weights <- (grid.street.weights -1) * -1

edge.weights = grid.street.weights * rail.weights * highway.weights * water.weights

write.csv(edge.weights, file="edge_weights.csv", row.names=FALSE)
