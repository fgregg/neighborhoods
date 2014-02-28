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


  


  
