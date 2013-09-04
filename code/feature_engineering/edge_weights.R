library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

physicalBarriers <- function(nodes) {
  centroids <- sp::coordinates(nodes)
  
  # Topology of Block Connectivity
  edges <-spdep::poly2nb(nodes,
                         foundInBox=rgeos::gUnarySTRtreeQuery(nodes))
  edgelist <- common::nb2edgelist(edges)

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
  edge.weights <- edge.weights * ((rail.weights - 1) * -1)

  highway.weights <- as.numeric(highway.intersects)
  edge.weights <- edge.weights * ((highway.weights - 1) * -1)

  water.weights <- as.numeric(water.intersects)
  edge.weights <- edge.weights * ((water.weights - 1) * -1)

  grid.street.weights <- as.numeric(grid.street.intersects)
  edge.weights <- edge.weights * (grid.street.weights -1) * -1

  weights <- list(edges=edgelist,
                  total=edge.weights,
                  rail=rail.weights,
                  highway=highway.weights,
                  water=water.weights,
                  grid_street=grid.street.weights)

  return(weights)
}


  
if (!common::from_source()) {
  weights <- physicalBarriers(block.groups.poly)
  write.csv(weights$edges, file="../interchange/edges.csv", row.names=FALSE)
  write.csv(weights$highway, file="../interchange/highway_intersects.csv", row.names=FALSE)
  write.csv(weights$water, file="../interchange/water_intersects.csv", row.names=FALSE)
  write.csv(weights$grid_street, file="../interchange/grid_intersects.csv", row.names=FALSE)
  write.csv(weights$rail, file="../interchange/rail_intersects.csv", row.names=FALSE)
  write.csv(weights$total, file="./edge_weights.csv", row.names=FALSE)
}


  
