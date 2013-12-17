library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

physicalBarriers <- function(nodes,
                             cached_edges=NULL,
                             railroads=NULL,
                             highways=NULL,
                             grid.streets=NULL,
                             water=NULL) {
  centroids <- sp::coordinates(nodes)

  edgelist <- common::edgeList(nodes, edges = cached_edges)

  edge.weights <- rep(1, dim(edgelist)[1])

  print("RAIL")

  railroad.intersects <- common::edgesIntersect(edgelist,
                                                centroids,
                                                railroads,
                                                projection)
  rail.distance <- common::distanceFromBorder(nodes,
                                              edgelist,
                                              as.numeric(railroad.intersects) + 1)
  print("HIGHWAY")
  

  highway.intersects <- common::edgesIntersect(edgelist,
                                               centroids,
                                               highways,
                                               projection)

  highway.distance <- common::distanceFromBorder(nodes,
                                                 edgelist,
                                                 as.numeric(highway.intersects) + 1)

  print("GRID STREET")
  
  grid.street.intersects <- common::edgesIntersect(edgelist,
                                                   centroids,
                                                   grid.streets,
                                                   projection)

  grid.distance <- common::distanceFromBorder(nodes,
                                              edgelist,
                                              as.numeric(grid.street.intersects) + 1)
  

  water.intersects <- common::edgesIntersect(edgelist,
                                             centroids,
                                             water,
                                             projection)

  water.distance <- common::distanceFromBorder(nodes,
                                               edgelist,
                                               as.numeric(water.intersects) + 1)


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
                  rail.distance=rail.distance,
                  highway=highway.weights,
                  highway.distance=highway.distance,
                  water=water.weights,
                  water.distance=water.distance,
                  grid_street=grid.street.weights,
                  grid.distance=grid.distance)

  return(weights)
}


  
if (!common::from_source()) {
  weights <- physicalBarriers(blocks.poly)
  write.csv(weights$edges, file="../interchange/edges.csv", row.names=FALSE)
  write.csv(weights$highway, file="../interchange/highway_intersects.csv", row.names=FALSE)
  write.csv(weights$water, file="../interchange/water_intersects.csv", row.names=FALSE)
  write.csv(weights$grid_street, file="../interchange/grid_intersects.csv", row.names=FALSE)
  write.csv(weights$rail, file="../interchange/rail_intersects.csv", row.names=FALSE
            )
  write.csv(weights$highway.distance, file="../interchange/highway_distance.csv", row.names=FALSE)
  write.csv(weights$water.distance, file="../interchange/water_distance.csv", row.names=FALSE)
  write.csv(weights$grid.distance, file="../interchange/grid_distance.csv", row.names=FALSE)
  write.csv(weights$rail.distance, file="../interchange/rail_distance.csv", row.names=FALSE
            )

  
  write.csv(weights$total, file="./edge_weights.csv", row.names=FALSE)
}


  
