library(devtools)
library(spdep)
library(rgeos)

pkg <- devtools::as.package('~/academic/neighborhoods/code/common')
devtools::load_all(pkg)

physicalBarriers <- function(nodes,
                             cached_edges=NULL,
                             edge_lines=NULL,
                             railroads=NULL,
                             highways=NULL,
                             grid.streets=NULL,
                             water=NULL) {

  edgelist <- common::edgeList(nodes, edges=cached_edges)
  
  edge.weights <- rep(1, dim(edgelist)[1])

  print("RAIL")

  railroad.intersects <- rowSums(rgeos::gIntersects(railroads,
                                                    edge_lines,
                                                    byid=TRUE))
  print("HIGHWAY")
  

  highway.intersects <- rowSums(rgeos::gIntersects(highways,
                                                   edge_lines,
                                                   byid=TRUE))

  print("GRID STREET")
  
  grid.street.intersects <- rowSums(rgeos::gIntersects(grid.streets,
                                                       edge_lines,
                                                       byid=TRUE))

  print("WATER")

  water.intersects <- rowSums(rgeos::gIntersects(water,
                                                 edge_lines,
                                                 byid=TRUE))


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


  
